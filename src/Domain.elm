module Domain exposing
    ( Active
    , Choice
    , Exercise
    , History
    , LiveSession
    , PlanItem
    , Preset
    , PresetItem
    , Segment
    , Session
    , activeElapsed
    , buildPlan
    , clampBpm
    , countInRemaining
    , dayKey
    , dedupeItems
    , displayName
    , exerciseTimeInLive
    , exerciseTotals
    , finish
    , fixedExercise
    , grouped
    , idleLimitMs
    , isIdle
    , isRunning
    , lastPlayed
    , lastTopBpm
    , liveTotal
    , minutesByDay
    , newLive
    , pause
    , reroll
    , rerollOptions
    , restore
    , resume
    , sessionTotal
    , setBpm
    , settleMs
    , startExercise
    , streak
    , topBpmHistory
    , topBpmsInSession
    , touch
    )

import Dict exposing (Dict)
import Random
import Time



-- MODEL


type alias Exercise =
    { id : String
    , name : String
    , startBpm : Int
    , subdivision : Int
    , metronome : Bool
    , notes : String
    , archived : Bool
    , parentId : Maybe String
    }


{-| One option of a preset slot. A random choice's `exerciseId` is a
generic exercise standing for any of its variants.
-}
type alias Choice =
    { exerciseId : String
    , minutes : Int
    , random : Bool
    }


{-| A preset slot: each session plays one of its choices (see `buildPlan`).
A fixed exercise is a slot with a single non-random choice.
-}
type alias PresetItem =
    { choices : List Choice
    }


{-| A preset slot resolved for one session. The slot's choices are kept so
the pick can be rerolled even if the preset is edited meanwhile.
-}
type alias PlanItem =
    { exerciseId : String
    , minutes : Int
    , choices : List Choice
    }


{-| When each exercise was last played, for favouring stale picks.
-}
type alias History =
    { now : Int
    , lastPlayed : Dict String Int
    }


type alias Preset =
    { id : String
    , name : String
    , items : List PresetItem
    }


{-| One continuous stretch on one exercise. Times are epoch millis.
-}
type alias Segment =
    { exerciseId : String
    , startedAt : Int
    , durationMs : Int
    , startBpm : Int
    , maxBpm : Int
    }


type alias Session =
    { id : String
    , startedAt : Int
    , endedAt : Int
    , presetId : Maybe String
    , segments : List Segment
    }


{-| The exercise currently on screen. `runningSince` may lie in the future
while a count-in plays: time only accrues once it has passed. `bpmMark` is
the elapsed time when the current BPM was set; a BPM only counts towards
`maxBpm` once it has been played for `settleMs` (so mis-taps don't count).
`maxBpm` is 0 until some BPM has been played that long.
-}
type alias Active =
    { exerciseId : String
    , bpm : Int
    , subdivision : Int
    , metronomeOn : Bool
    , startBpm : Int
    , maxBpm : Int
    , startedAt : Int
    , accumulatedMs : Int
    , runningSince : Maybe Int
    , bpmMark : Int
    }


type alias LiveSession =
    { id : String
    , startedAt : Int
    , presetId : Maybe String
    , segments : List Segment
    , active : Maybe Active
    , chimed : List String
    , heartbeat : Int
    , plan : List PlanItem
    }


{-| "Лик · Blues" for a variant, the plain name otherwise.
-}
displayName : List Exercise -> Exercise -> String
displayName exercises ex =
    case ex.parentId |> Maybe.andThen (\pid -> List.filter (\p -> p.id == pid) exercises |> List.head) of
        Just parent ->
            parent.name ++ " · " ++ ex.name

        Nothing ->
            ex.name


{-| Library order: each generic exercise with its variants. Variants whose
parent is gone are listed as generics.
-}
grouped : List Exercise -> List ( Exercise, List Exercise )
grouped exercises =
    let
        isTop e =
            case e.parentId of
                Just pid ->
                    not (List.any (\p -> p.id == pid) exercises)

                Nothing ->
                    True
    in
    exercises
        |> List.filter isTop
        |> List.map (\top -> ( top, List.filter (\e -> e.parentId == Just top.id) exercises ))


{-| Tidy a preset: no repeated choice within a slot, no empty slots, and a
fixed exercise at most once (later repeats are dropped).
-}
dedupeItems : List PresetItem -> List PresetItem
dedupeItems items =
    let
        uniqueChoices =
            List.foldl
                (\c acc ->
                    if List.any (\x -> x.exerciseId == c.exerciseId && x.random == c.random) acc then
                        acc

                    else
                        acc ++ [ c ]
                )
                []
    in
    items
        |> List.map (\i -> { i | choices = uniqueChoices i.choices })
        |> List.filter (not << List.isEmpty << .choices)
        |> List.foldl
            (\item acc ->
                case fixedExercise item of
                    Just id ->
                        if List.any (\i -> fixedExercise i == Just id) acc then
                            acc

                        else
                            acc ++ [ item ]

                    Nothing ->
                        acc ++ [ item ]
            )
            []


{-| The exercise of a fixed (single, non-random choice) slot.
-}
fixedExercise : { a | choices : List { b | exerciseId : String, random : Bool } } -> Maybe String
fixedExercise item =
    case item.choices of
        [ c ] ->
            if c.random then
                Nothing

            else
                Just c.exerciseId

        _ ->
            Nothing


{-| What a random choice can pick: the generic's variants, or the generic
itself when it has none.
-}
groupMembers : List Exercise -> String -> List String
groupMembers exercises groupId =
    case List.filter (\e -> e.parentId == Just groupId && not e.archived) exercises of
        [] ->
            List.filter (\e -> e.id == groupId && not e.archived) exercises |> List.map .id

        variants ->
            List.map .id variants


outcomes : List Exercise -> Choice -> List String
outcomes exercises c =
    if c.random then
        groupMembers exercises c.exerciseId

    else
        [ c.exerciseId ]


{-| Every exercise a slot can end up as.
-}
slotOutcomes : List Exercise -> List Choice -> List String
slotOutcomes exercises choices =
    List.foldl
        (\id acc ->
            if List.member id acc then
                acc

            else
                acc ++ [ id ]
        )
        []
        (List.concatMap (outcomes exercises) choices)


lastPlayed : List Session -> Dict String Int
lastPlayed sessions =
    List.foldl
        (\seg -> Dict.update seg.exerciseId (Maybe.withDefault 0 >> max seg.startedAt >> Just))
        Dict.empty
        (List.concatMap .segments sessions)


{-| 1 for played just now, growing a point a day up to 15 (also for never
played). A group counts as played when any of its exercises was.
-}
staleness : History -> List String -> Float
staleness history ids =
    case List.filterMap (\id -> Dict.get id history.lastPlayed) ids |> List.maximum of
        Just at ->
            1 + clamp 0 14 (toFloat (history.now - at) / 86400000)

        Nothing ->
            15


weightedPick : Random.Seed -> List ( Float, a ) -> Maybe ( a, Random.Seed )
weightedPick seed options =
    case options of
        [] ->
            Nothing

        x :: xs ->
            Just (Random.step (Random.weighted x xs) seed)


{-| Pick a choice, then (for a random choice) one of its exercises, both
favouring stale ones. Exercises in `taken` are avoided.
-}
pickSlot : History -> List Exercise -> List String -> Random.Seed -> List Choice -> Maybe ( ( String, Int ), Random.Seed )
pickSlot history exercises taken seed choices =
    choices
        |> List.filterMap
            (\c ->
                case List.filter (\id -> not (List.member id taken)) (outcomes exercises c) of
                    [] ->
                        Nothing

                    ids ->
                        Just ( staleness history (outcomes exercises c), ( c, ids ) )
            )
        |> weightedPick seed
        |> Maybe.andThen
            (\( ( c, ids ), next ) ->
                weightedPick next (List.map (\id -> ( staleness history [ id ], id )) ids)
                    |> Maybe.map (\( id, last ) -> ( ( id, c.minutes ), last ))
            )


{-| Resolve a preset for one session, avoiding exercises already in the plan
and those fixed elsewhere in the preset. When a slot has nothing left the
pick repeats and its minutes join the existing row.
-}
buildPlan : History -> Random.Seed -> List Exercise -> List PresetItem -> List PlanItem
buildPlan history seed0 exercises items =
    let
        fixed =
            List.filterMap fixedExercise items

        add item plan =
            if List.any (\p -> p.exerciseId == item.exerciseId) plan then
                List.map
                    (\p ->
                        if p.exerciseId == item.exerciseId then
                            { p | minutes = p.minutes + item.minutes }

                        else
                            p
                    )
                    plan

            else
                plan ++ [ item ]

        step item ( seed, plan ) =
            let
                taken =
                    List.map .exerciseId plan
                        ++ (if fixedExercise item == Nothing then
                                fixed

                            else
                                []
                           )

                picked =
                    case pickSlot history exercises taken seed item.choices of
                        Nothing ->
                            pickSlot history exercises [] seed item.choices

                        found ->
                            found
            in
            case picked of
                Just ( ( id, minutes ), next ) ->
                    ( next, add { exerciseId = id, minutes = minutes, choices = item.choices } plan )

                Nothing ->
                    ( seed, plan )
    in
    List.foldl step ( seed0, [] ) items |> Tuple.second


{-| Other picks for plan row `idx`: its slot's exercises not in the plan yet.
-}
rerollOptions : List Exercise -> Int -> List PlanItem -> List String
rerollOptions exercises idx plan =
    case List.drop idx plan |> List.head of
        Just item ->
            let
                taken =
                    List.map .exerciseId plan
            in
            slotOutcomes exercises item.choices |> List.filter (\id -> not (List.member id taken))

        Nothing ->
            []


reroll : History -> Random.Seed -> List Exercise -> Int -> List PlanItem -> List PlanItem
reroll history seed exercises idx plan =
    case List.drop idx plan |> List.head of
        Just item ->
            case pickSlot history exercises (List.map .exerciseId plan) seed item.choices of
                Just ( ( id, minutes ), _ ) ->
                    List.indexedMap
                        (\i p ->
                            if i == idx then
                                { p | exerciseId = id, minutes = minutes }

                            else
                                p
                        )
                        plan

                Nothing ->
                    plan

        Nothing ->
            plan



-- LIVE SESSION


idleLimitMs : Int
idleLimitMs =
    30 * 60 * 1000


newLive : Int -> Maybe String -> LiveSession
newLive now presetId =
    { id = "s" ++ String.fromInt now
    , startedAt = now
    , presetId = presetId
    , segments = []
    , active = Nothing
    , chimed = []
    , heartbeat = now
    , plan = []
    }


clampBpm : Int -> Int
clampBpm =
    clamp 20 400


activeElapsed : Int -> Active -> Int
activeElapsed now a =
    case a.runningSince of
        Just since ->
            a.accumulatedMs + max 0 (now - since)

        Nothing ->
            a.accumulatedMs


isRunning : LiveSession -> Bool
isRunning live =
    case live.active of
        Just a ->
            a.runningSince /= Nothing

        Nothing ->
            False


countInRemaining : Int -> Active -> Int
countInRemaining now a =
    case a.runningSince of
        Just since ->
            max 0 (since - now)

        Nothing ->
            0


settleMs : Int
settleMs =
    10 * 1000


settle : Int -> Active -> Active
settle now a =
    if activeElapsed now a - a.bpmMark >= settleMs then
        { a | maxBpm = max a.maxBpm a.bpm }

    else
        a


pause : Int -> Active -> Active
pause now a =
    let
        settled =
            settle now a
    in
    { settled | accumulatedMs = activeElapsed now a, runningSince = Nothing }


{-| Resume with a one-bar (4 beat) count-in when the click is on.
-}
resume : Int -> Active -> Active
resume now a =
    let
        countIn =
            if a.metronomeOn then
                4 * 60000 // a.bpm

            else
                0
    in
    case a.runningSince of
        Just _ ->
            a

        Nothing ->
            { a | runningSince = Just (now + countIn) }


setBpm : Int -> Int -> Active -> Active
setBpm now bpm a =
    let
        settled =
            settle now a
    in
    { settled | bpm = clampBpm bpm, bpmMark = activeElapsed now a }


closeActive : Int -> LiveSession -> LiveSession
closeActive now live =
    case live.active of
        Just running ->
            let
                a =
                    settle now running

                elapsed =
                    activeElapsed now a

                segments =
                    if elapsed >= 1000 then
                        live.segments
                            ++ [ { exerciseId = a.exerciseId
                                 , startedAt = a.startedAt
                                 , durationMs = elapsed
                                 , startBpm = a.startBpm
                                 , maxBpm = a.maxBpm
                                 }
                               ]

                    else
                        live.segments
            in
            { live | segments = segments, active = Nothing }

        Nothing ->
            live


{-| Switch to an exercise. It always starts paused.
-}
startExercise : Int -> Exercise -> LiveSession -> LiveSession
startExercise now ex live =
    if Maybe.map .exerciseId live.active == Just ex.id then
        live

    else
        let
            closed =
                closeActive now live
        in
        { closed
            | active =
                Just
                    { exerciseId = ex.id
                    , bpm = ex.startBpm
                    , subdivision = ex.subdivision
                    , metronomeOn = ex.metronome
                    , startBpm = ex.startBpm
                    , maxBpm = 0
                    , startedAt = now
                    , accumulatedMs = 0
                    , runningSince = Nothing
                    , bpmMark = 0
                    }
        }


{-| Record that the app was alive and playing at `now`.
-}
touch : Int -> LiveSession -> LiveSession
touch now live =
    if isRunning live then
        { live | heartbeat = now, active = Maybe.map (settle now) live.active }

    else
        live


{-| After a reload: a segment left running is closed at the last heartbeat,
so a killed app never inflates practice time.
-}
restore : LiveSession -> LiveSession
restore live =
    { live | active = Maybe.map (pause live.heartbeat) live.active }


isIdle : Int -> LiveSession -> Bool
isIdle now live =
    not (isRunning live) && now - live.heartbeat > idleLimitMs


exerciseTimeInLive : Int -> String -> LiveSession -> Int
exerciseTimeInLive now exerciseId live =
    let
        closed =
            live.segments
                |> List.filter (\s -> s.exerciseId == exerciseId)
                |> List.map .durationMs
                |> List.sum

        current =
            case live.active of
                Just a ->
                    if a.exerciseId == exerciseId then
                        activeElapsed now a

                    else
                        0

                Nothing ->
                    0
    in
    closed + current


liveTotal : Int -> LiveSession -> Int
liveTotal now live =
    List.sum (List.map .durationMs live.segments)
        + (live.active |> Maybe.map (activeElapsed now) |> Maybe.withDefault 0)


{-| Close the session. `Nothing` when nothing was practiced.
-}
finish : Int -> LiveSession -> Maybe Session
finish now live =
    let
        closed =
            closeActive now live
    in
    if List.isEmpty closed.segments then
        Nothing

    else
        Just
            { id = live.id
            , startedAt = live.startedAt
            , endedAt = now
            , presetId = live.presetId
            , segments = closed.segments
            }



-- STATS


sessionTotal : Session -> Int
sessionTotal s =
    List.sum (List.map .durationMs s.segments)


dayKey : Time.Zone -> Int -> Int
dayKey zone millis =
    let
        t =
            Time.millisToPosix millis
    in
    Time.toYear zone t * 10000 + monthNumber (Time.toMonth zone t) * 100 + Time.toDay zone t


monthNumber : Time.Month -> Int
monthNumber m =
    case m of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


{-| Practice millis per local day (keyed by `dayKey`), attributed to the day
each segment started.
-}
minutesByDay : Time.Zone -> List Session -> Dict Int Int
minutesByDay zone sessions =
    sessions
        |> List.concatMap .segments
        |> List.foldl
            (\seg acc ->
                Dict.update (dayKey zone seg.startedAt)
                    (\v -> Just (Maybe.withDefault 0 v + seg.durationMs))
                    acc
            )
            Dict.empty


{-| Consecutive days with practice, ending today (or yesterday, so the streak
doesn't read 0 before today's session).
-}
streak : Time.Zone -> Int -> List Session -> Int
streak zone now sessions =
    let
        days =
            minutesByDay zone sessions

        dayMs =
            24 * 60 * 60 * 1000

        practiced offset =
            Dict.member (dayKey zone (now - offset * dayMs)) days

        count offset acc =
            if practiced offset then
                count (offset + 1) (acc + 1)

            else
                acc
    in
    if practiced 0 then
        count 0 0

    else
        count 1 0


exerciseTotals : List Session -> Dict String Int
exerciseTotals sessions =
    sessions
        |> List.concatMap .segments
        |> List.foldl
            (\seg acc ->
                Dict.update seg.exerciseId
                    (\v -> Just (Maybe.withDefault 0 v + seg.durationMs))
                    acc
            )
            Dict.empty


topBpmsInSession : Session -> Dict String Int
topBpmsInSession s =
    s.segments
        |> List.filter (\seg -> seg.maxBpm > 0)
        |> List.foldl
            (\seg acc ->
                Dict.update seg.exerciseId
                    (\v -> Just (max seg.maxBpm (Maybe.withDefault 0 v)))
                    acc
            )
            Dict.empty


{-| (session start, top BPM) for every session containing the exercise,
oldest first. Sessions must be passed newest first.
-}
topBpmHistory : String -> List Session -> List ( Int, Int )
topBpmHistory exerciseId sessions =
    sessions
        |> List.filterMap
            (\s ->
                Dict.get exerciseId (topBpmsInSession s)
                    |> Maybe.map (\bpm -> ( s.startedAt, bpm ))
            )
        |> List.reverse


{-| Top BPM from the most recent session containing the exercise.
Sessions must be passed newest first.
-}
lastTopBpm : String -> List Session -> Maybe Int
lastTopBpm exerciseId sessions =
    case sessions of
        [] ->
            Nothing

        s :: rest ->
            case Dict.get exerciseId (topBpmsInSession s) of
                Just bpm ->
                    Just bpm

                Nothing ->
                    lastTopBpm exerciseId rest
