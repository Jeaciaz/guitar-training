module StoreTest exposing (suite)

import Domain
import Expect
import Json.Decode as D
import Json.Encode as E
import Store
import Test exposing (Test, describe, test)


sample : Store.Store
sample =
    let
        base =
            Store.empty

        newLive =
            Domain.newLive 5 (Just "p1")

        live =
            { newLive | plan = [ { exerciseId = "free", minutes = 5, choices = [ { exerciseId = "free", minutes = 5, random = True } ] } ] }
                |> Domain.startExercise 5 { id = "free", name = "Свободная игра", startBpm = 100, subdivision = 1, metronome = False, notes = "", archived = False, parentId = Nothing }
    in
    { base
        | exercises = List.filter (\e -> e.id == "free") base.exercises
        , presets = [ { id = "p1", name = "Daily", items = [ { choices = [ { exerciseId = "free", minutes = 5, random = True } ] } ] } ]
        , sessions =
            [ { id = "s1"
              , startedAt = 1
              , endedAt = 2
              , presetId = Nothing
              , segments = [ { exerciseId = "free", startedAt = 1, durationMs = 1000, startBpm = 100, maxBpm = 104 } ]
              }
            ]
        , live = Just live
    }


suite : Test
suite =
    describe "Store"
        [ test "JSON roundtrip" <|
            \_ ->
                Store.encode sample
                    |> E.encode 0
                    |> D.decodeString Store.decoder
                    |> Expect.equal (Ok sample)
        , test "merge import adds exercises and resolves presets by name" <|
            \_ ->
                let
                    json =
                        """{"exercises":[{"name":"Sweeps","bpm":90,"subdivision":3}],
                            "presets":[{"name":"Evening","items":[{"exercise":"sweeps","minutes":10},{"exercise":"свободная ИГРА","minutes":5}]}]}"""
                in
                Store.mergeImport 100 json sample
                    |> Result.map
                        (\s ->
                            ( List.map (\e -> ( e.name, e.startBpm, e.subdivision )) s.exercises
                            , List.map (\p -> ( p.name, List.concatMap (.choices >> List.map .minutes) p.items )) s.presets
                            , List.length s.sessions
                            )
                        )
                    |> Expect.equal
                        (Ok
                            ( [ ( "Свободная игра", 100, 1 ), ( "Sweeps", 90, 3 ) ]
                            , [ ( "Daily", [ 5 ] ), ( "Evening", [ 10, 5 ] ) ]
                            , 1
                            )
                        )
        , test "merge import updates an existing exercise by name" <|
            \_ ->
                Store.mergeImport 100 """{"exercises":[{"name":"СВОБОДНАЯ игра","bpm":70}]}""" sample
                    |> Result.map (.exercises >> List.map (\e -> ( e.id, e.startBpm )))
                    |> Expect.equal (Ok [ ( "free", 70 ) ])
        , test "merge import keeps each exercise once per preset" <|
            \_ ->
                Store.mergeImport 100 """{"presets":[{"name":"X","items":[{"exercise":"Свободная игра","minutes":3},{"exercise":"свободная игра","minutes":7}]}]}""" sample
                    |> Result.map (.presets >> List.filter (\p -> p.name == "X") >> List.map (.items >> List.concatMap (.choices >> List.map .minutes)))
                    |> Expect.equal (Ok [ [ 3 ] ])
        , test "merge import links variants and resolves them by display name" <|
            \_ ->
                let
                    json =
                        """{"exercises":[{"name":"Лик"},{"name":"Blues","parent":"лик","bpm":90},{"name":"Blues","bpm":70}],
                            "presets":[{"name":"P","items":[{"exercise":"Лик · Blues","minutes":5}]}]}"""
                in
                Store.mergeImport 100 json sample
                    |> Result.map
                        (\s ->
                            ( List.map (\e -> ( Domain.displayName s.exercises e, e.startBpm )) s.exercises
                            , s.presets |> List.filter (\pr -> pr.name == "P") |> List.concatMap .items |> List.concatMap .choices |> List.map .exerciseId
                            )
                        )
                    |> Expect.equal
                        (Ok
                            ( [ ( "Свободная игра", 100 ), ( "Лик", 60 ), ( "Лик · Blues", 90 ), ( "Blues", 70 ) ]
                            , [ "e101" ]
                            )
                        )
        , test "merge import rejects an unknown parent" <|
            \_ ->
                Store.mergeImport 100 """{"exercises":[{"name":"Blues","parent":"Nope"}]}""" sample
                    |> Expect.equal (Err "Неизвестное родительское упражнение: Nope")
        , test "the standard library has no dangling preset items or parents" <|
            \_ ->
                let
                    ids =
                        List.map .id Store.empty.exercises

                    generics =
                        Store.empty.exercises |> List.filter (\e -> e.parentId == Nothing) |> List.map .id
                in
                Expect.equal
                    ( List.all
                        (\i ->
                            List.member i.exerciseId
                                (if i.random then
                                    generics

                                 else
                                    ids
                                )
                        )
                        (List.concatMap (.items >> List.concatMap .choices) Store.empty.presets)
                    , List.all (\e -> Maybe.map (\pid -> List.member pid ids) e.parentId |> Maybe.withDefault True) Store.empty.exercises
                    )
                    ( True, True )
        , test "merge import reports unknown exercises" <|
            \_ ->
                Store.mergeImport 100 """{"presets":[{"name":"X","items":[{"exercise":"Nope","minutes":1}]}]}""" sample
                    |> Expect.equal (Err "Неизвестные упражнения в пресетах: Nope")
        , test "merge import takes random items by generic name" <|
            \_ ->
                let
                    json =
                        """{"exercises":[{"name":"Лик"},{"name":"Blues","parent":"Лик"}],
                            "presets":[{"name":"P","items":[{"exercise":"лик","random":true,"minutes":5},{"exercise":"Лик","random":true,"minutes":7}]}]}"""
                in
                Store.mergeImport 100 json sample
                    |> Result.map (.presets >> List.filter (\pr -> pr.name == "P") >> List.concatMap .items)
                    |> Expect.equal
                        (Ok [ { choices = [ { exerciseId = "e100", minutes = 5, random = True } ] }, { choices = [ { exerciseId = "e100", minutes = 7, random = True } ] } ])
        , test "merge import takes slots with several choices" <|
            \_ ->
                let
                    json =
                        """{"exercises":[{"name":"Лик"}],
                            "presets":[{"name":"P","items":[{"choices":[{"exercise":"лик","random":true,"minutes":5},{"exercise":"Свободная игра","minutes":3}]}]}]}"""
                in
                Store.mergeImport 100 json sample
                    |> Result.map (.presets >> List.filter (\pr -> pr.name == "P") >> List.concatMap .items)
                    |> Expect.equal
                        (Ok [ { choices = [ { exerciseId = "e100", minutes = 5, random = True }, { exerciseId = "free", minutes = 3, random = False } ] } ])
        , test "data saved before random items and plans still loads" <|
            \_ ->
                """{"version":1,"exercises":[],"sessions":[],"settings":{"volume":1},
                    "presets":[{"id":"p","name":"P","items":[{"exerciseId":"x","minutes":3}]}],
                    "live":{"id":"s","startedAt":1,"presetId":"p","segments":[],"active":null,"chimed":[],"heartbeat":1}}"""
                    |> D.decodeString Store.decoder
                    |> Result.map (\s -> ( List.concatMap .items s.presets, Maybe.map .plan s.live ))
                    |> Expect.equal (Ok ( [ { choices = [ { exerciseId = "x", minutes = 3, random = False } ] } ], Just [] ))
        , test "a full backup replaces everything" <|
            \_ ->
                Store.mergeImport 100 (E.encode 0 (Store.encode sample)) Store.empty
                    |> Expect.equal (Ok sample)
        ]
