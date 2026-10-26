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

        live =
            Domain.newLive 5 (Just "p1")
                |> Domain.startExercise 5 { id = "free", name = "Свободная игра", startBpm = 100, subdivision = 1, metronome = False, notes = "", archived = False, parentId = Nothing }
    in
    { base
        | exercises = List.filter (\e -> e.id == "free") base.exercises
        , presets = [ { id = "p1", name = "Daily", items = [ { exerciseId = "free", minutes = 5 } ] } ]
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
                            , List.map (\p -> ( p.name, List.map .minutes p.items )) s.presets
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
                    |> Result.map (.presets >> List.filter (\p -> p.name == "X") >> List.map (.items >> List.map .minutes))
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
                            , s.presets |> List.filter (\pr -> pr.name == "P") |> List.concatMap .items |> List.map .exerciseId
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
                in
                Expect.equal
                    ( List.all (\i -> List.member i.exerciseId ids) (List.concatMap .items Store.empty.presets)
                    , List.all (\e -> Maybe.map (\pid -> List.member pid ids) e.parentId |> Maybe.withDefault True) Store.empty.exercises
                    )
                    ( True, True )
        , test "merge import reports unknown exercises" <|
            \_ ->
                Store.mergeImport 100 """{"presets":[{"name":"X","items":[{"exercise":"Nope","minutes":1}]}]}""" sample
                    |> Expect.equal (Err "Неизвестные упражнения в пресетах: Nope")
        , test "a full backup replaces everything" <|
            \_ ->
                Store.mergeImport 100 (E.encode 0 (Store.encode sample)) Store.empty
                    |> Expect.equal (Ok sample)
        ]
