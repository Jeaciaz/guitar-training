module DomainTest exposing (suite)

import Dict
import Domain exposing (Exercise)
import Expect
import Random
import Test exposing (Test, describe, test)
import Time


ex : String -> Int -> Bool -> Exercise
ex id bpm click =
    { id = id, name = id, startBpm = bpm, subdivision = 1, metronome = click, notes = "", archived = False, parentId = Nothing }


sweeps : Exercise
sweeps =
    ex "sweeps" 120 True


free : Exercise
free =
    ex "free" 100 False


variant : String -> String -> Exercise
variant parent id =
    { id = id, name = id, startBpm = 60, subdivision = 1, metronome = True, notes = "", archived = False, parentId = Just parent }


{-| seq has three variants, riff none, lick one (plus an archived one).
-}
groups : List Exercise
groups =
    [ ex "seq" 60 True
    , variant "seq" "a"
    , variant "seq" "b"
    , variant "seq" "c"
    , ex "riff" 60 True
    , ex "lick" 60 True
    , variant "lick" "blues"
    , { id = "old", name = "old", startBpm = 60, subdivision = 1, metronome = True, notes = "", archived = True, parentId = Just "lick" }
    ]


choice : String -> Int -> Bool -> Domain.Choice
choice id minutes random =
    { exerciseId = id, minutes = minutes, random = random }


{-| A one-choice slot.
-}
item : String -> Int -> Bool -> Domain.PresetItem
item id minutes random =
    { choices = [ choice id minutes random ] }


fresh : Domain.History
fresh =
    { now = 0, lastPlayed = Dict.empty }


{-| Plans for many seeds, as (exerciseId, minutes) rows.
-}
plansWith : Domain.History -> List Domain.PresetItem -> List (List ( String, Int ))
plansWith history items =
    List.range 0 199
        |> List.map (\n -> Domain.buildPlan history (Random.initialSeed n) groups items |> List.map (\p -> ( p.exerciseId, p.minutes )))


plans : List Domain.PresetItem -> List (List ( String, Int ))
plans =
    plansWith fresh


{-| Run steps on the active exercise of a live session.
-}
onActive : (Domain.Active -> Domain.Active) -> Domain.LiveSession -> Domain.LiveSession
onActive f live =
    { live | active = Maybe.map f live.active }


suite : Test
suite =
    describe "Domain"
        [ test "new exercise starts paused at its start BPM" <|
            \_ ->
                Domain.newLive 0 Nothing
                    |> Domain.startExercise 0 sweeps
                    |> .active
                    |> Maybe.map (\a -> ( a.bpm, a.runningSince ))
                    |> Expect.equal (Just ( 120, Nothing ))
        , test "count-in delays the timer by one bar when the click is on" <|
            \_ ->
                -- 4 beats at 120 BPM = 2000 ms
                Domain.newLive 0 Nothing
                    |> Domain.startExercise 0 sweeps
                    |> onActive (Domain.resume 0)
                    |> Domain.liveTotal 5000
                    |> Expect.equal 3000
        , test "no count-in without a click" <|
            \_ ->
                Domain.newLive 0 Nothing
                    |> Domain.startExercise 0 free
                    |> onActive (Domain.resume 0)
                    |> Domain.liveTotal 5000
                    |> Expect.equal 5000
        , test "paused time does not count" <|
            \_ ->
                Domain.newLive 0 Nothing
                    |> Domain.startExercise 0 free
                    |> onActive (Domain.resume 0)
                    |> onActive (Domain.pause 10000)
                    |> onActive (Domain.resume 60000)
                    |> Domain.liveTotal 65000
                    |> Expect.equal 15000
        , test "switching exercises keeps per-exercise and session totals" <|
            \_ ->
                let
                    live =
                        Domain.newLive 0 Nothing
                            |> Domain.startExercise 0 free
                            |> onActive (Domain.resume 0)
                            |> Domain.startExercise 30000 sweeps
                            |> onActive (Domain.resume 30000)
                            |> Domain.startExercise 60000 free
                            |> onActive (Domain.resume 60000)
                in
                Expect.equal
                    ( Domain.exerciseTimeInLive 70000 "free" live
                    , Domain.exerciseTimeInLive 70000 "sweeps" live
                    , Domain.liveTotal 70000 live
                    )
                    ( 40000, 28000, 68000 )
        , test "restore after a kill closes the running segment at the heartbeat" <|
            \_ ->
                Domain.newLive 0 Nothing
                    |> Domain.startExercise 0 free
                    |> onActive (Domain.resume 0)
                    |> Domain.touch 20000
                    |> Domain.restore
                    |> Domain.liveTotal 9999999
                    |> Expect.equal 20000
        , test "session is idle after 30 paused minutes" <|
            \_ ->
                let
                    live =
                        Domain.newLive 0 Nothing |> Domain.startExercise 0 free
                in
                Expect.equal
                    ( Domain.isIdle (Domain.idleLimitMs - 1) live, Domain.isIdle (Domain.idleLimitMs + 1) live )
                    ( False, True )
        , test "a BPM counts as top only after being played for a while" <|
            \_ ->
                let
                    live =
                        Domain.newLive 0 Nothing
                            |> Domain.startExercise 0 free
                            |> onActive (Domain.resume 0)
                            |> onActive (Domain.setBpm 1000 130)
                            -- mis-tap: 130 for 2 s
                            |> onActive (Domain.setBpm 3000 110)
                            -- 110 played for 20 s
                            |> onActive (Domain.setBpm 23000 105)
                in
                Domain.finish 40000 live
                    |> Maybe.map (.segments >> List.map .maxBpm)
                    |> Expect.equal (Just [ 110 ])
        , test "top BPM can be below the start BPM" <|
            \_ ->
                Domain.newLive 0 Nothing
                    |> Domain.startExercise 0 sweeps
                    |> onActive (Domain.setBpm 0 100)
                    |> onActive (Domain.resume 0)
                    |> Domain.finish 60000
                    |> Maybe.map (.segments >> List.map .maxBpm)
                    |> Expect.equal (Just [ 100 ])
        , test "a too-short stretch records no top BPM" <|
            \_ ->
                Domain.newLive 0 Nothing
                    |> Domain.startExercise 0 free
                    |> onActive (Domain.resume 0)
                    |> Domain.finish 5000
                    |> Maybe.map (.segments >> List.map .maxBpm)
                    |> Expect.equal (Just [ 0 ])
        , test "finish without practice yields no session" <|
            \_ ->
                Domain.newLive 0 Nothing
                    |> Domain.startExercise 0 free
                    |> Domain.finish 50000
                    |> Expect.equal Nothing
        , test "lastTopBpm looks at the most recent session with the exercise" <|
            \_ ->
                let
                    session id at bpm =
                        { id = id
                        , startedAt = at
                        , endedAt = at
                        , presetId = Nothing
                        , segments = [ { exerciseId = id, startedAt = at, durationMs = 1000, startBpm = 100, maxBpm = bpm } ]
                        }
                in
                Domain.lastTopBpm "sweeps" [ session "free" 3 90, session "sweeps" 2 132, session "sweeps" 1 120 ]
                    |> Expect.equal (Just 132)
        , test "random items pick distinct live variants, never a fixed one" <|
            \_ ->
                plans [ item "a" 3 False, item "seq" 10 True, item "seq" 7 True, item "lick" 5 True ]
                    |> List.all
                        (\plan ->
                            case plan of
                                [ ( "a", 3 ), ( x, 10 ), ( y, 7 ), ( "blues", 5 ) ] ->
                                    x /= y && List.member x [ "b", "c" ] && List.member y [ "b", "c" ]

                                _ ->
                                    False
                        )
                    |> Expect.equal True
        , test "random picks vary between seeds" <|
            \_ ->
                plans [ item "seq" 10 True ]
                    |> List.filterMap (List.head >> Maybe.map Tuple.first)
                    |> List.sort
                    |> List.foldr
                        (\x acc ->
                            if List.head acc == Just x then
                                acc

                            else
                                x :: acc
                        )
                        []
                    |> Expect.equal [ "a", "b", "c" ]
        , test "a slot plays one of its choices, for that choice's minutes" <|
            \_ ->
                let
                    results =
                        plans [ { choices = [ choice "seq" 8 True, choice "riff" 5 True ] } ]
                            |> List.concat
                in
                ( List.all (\( id, m ) -> (List.member id [ "a", "b", "c" ] && m == 8) || (id == "riff" && m == 5)) results
                , List.any (\( id, _ ) -> id == "riff") results && List.any (\( id, _ ) -> id /= "riff") results
                )
                    |> Expect.equal ( True, True )
        , test "stale exercises come up more often" <|
            \_ ->
                let
                    now =
                        30 * 86400000

                    -- a played today (weight 1), b and c never (15 each)
                    history =
                        { now = now, lastPlayed = Dict.fromList [ ( "a", now ) ] }
                in
                plansWith history [ item "seq" 10 True ]
                    |> List.filter (List.head >> Maybe.map Tuple.first >> (==) (Just "a"))
                    |> List.length
                    |> Expect.lessThan 20
        , test "a group without variants plays the generic; an exhausted group merges minutes" <|
            \_ ->
                Domain.buildPlan fresh (Random.initialSeed 1) groups [ item "riff" 5 True, item "lick" 5 True, item "lick" 3 True, item "gone" 4 True ]
                    |> List.map (\p -> ( p.exerciseId, p.minutes ))
                    |> Expect.equal [ ( "riff", 5 ), ( "blues", 8 ) ]
        , test "reroll swaps to another free pick and its minutes, or keeps the pick" <|
            \_ ->
                let
                    row id minutes choices =
                        { exerciseId = id, minutes = minutes, choices = choices }

                    plan =
                        [ row "a" 5 [ choice "seq" 5 True ]
                        , row "riff" 4 [ choice "riff" 4 True, choice "b" 9 False ]
                        , row "blues" 5 [ choice "lick" 5 True ]
                        ]

                    reroll idx =
                        Domain.reroll fresh (Random.initialSeed 7) groups idx plan |> List.map (\p -> ( p.exerciseId, p.minutes ))
                in
                ( reroll 1, reroll 2 )
                    |> Expect.equal
                        ( [ ( "a", 5 ), ( "b", 9 ), ( "blues", 5 ) ]
                        , [ ( "a", 5 ), ( "riff", 4 ), ( "blues", 5 ) ]
                        )
        , test "reroll of a random choice avoids picks already in the plan" <|
            \_ ->
                Domain.reroll fresh
                    (Random.initialSeed 3)
                    groups
                    0
                    [ { exerciseId = "a", minutes = 5, choices = [ choice "seq" 5 True ] }
                    , { exerciseId = "b", minutes = 5, choices = [ choice "seq" 5 True ] }
                    ]
                    |> List.map .exerciseId
                    |> Expect.equal [ "c", "b" ]
        , test "dedupe tidies slots and keeps a fixed exercise once" <|
            \_ ->
                Domain.dedupeItems
                    [ item "a" 1 False
                    , { choices = [ choice "seq" 2 True, choice "seq" 2 True, choice "a" 3 False ] }
                    , item "a" 3 False
                    , { choices = [] }
                    , item "seq" 4 True
                    ]
                    |> List.map (.choices >> List.map .minutes)
                    |> Expect.equal [ [ 1 ], [ 2, 3 ], [ 4 ] ]
        , test "minutesByDay buckets by local day" <|
            \_ ->
                let
                    -- 2026-10-25 23:30 UTC is already the 26th in UTC+2
                    at =
                        1792971000000

                    sessions =
                        [ { id = "s"
                          , startedAt = at
                          , endedAt = at
                          , presetId = Nothing
                          , segments = [ { exerciseId = "x", startedAt = at, durationMs = 60000, startBpm = 1, maxBpm = 1 } ]
                          }
                        ]
                in
                Expect.equal
                    ( Dict.keys (Domain.minutesByDay Time.utc sessions)
                    , Dict.keys (Domain.minutesByDay (Time.customZone 120 []) sessions)
                    )
                    ( [ 20261025 ], [ 20261026 ] )
        ]
