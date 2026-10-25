module Domain exposing
    ( Active
    , Exercise
    , LiveSession
    , Preset
    , PresetItem
    , Segment
    , Session
    , activeElapsed
    , clampBpm
    , countInRemaining
    , dayKey
    , exerciseTimeInLive
    , exerciseTotals
    , finish
    , idleLimitMs
    , isIdle
    , isRunning
    , lastTopBpm
    , liveTotal
    , minutesByDay
    , newLive
    , pause
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
    }


type alias PresetItem =
    { exerciseId : String
    , minutes : Int
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
    }



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
                    , maxBpm = ex.startBpm
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
    List.foldl
        (\seg acc ->
            Dict.update seg.exerciseId
                (\v -> Just (max seg.maxBpm (Maybe.withDefault 0 v)))
                acc
        )
        Dict.empty
        s.segments


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
