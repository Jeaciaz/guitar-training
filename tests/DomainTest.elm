module DomainTest exposing (suite)

import Dict
import Domain exposing (Exercise)
import Expect
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
