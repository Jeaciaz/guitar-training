module Store exposing
    ( Settings
    , Store
    , decoder
    , empty
    , encode
    , mergeImport
    )

{-| Everything persisted to localStorage, its JSON codec, and the lenient
"paste JSON" merge import (see README for the import format).
-}

import Domain exposing (Active, Exercise, LiveSession, Preset, PresetItem, Segment, Session)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type alias Settings =
    { volume : Float
    }


{-| `sessions` are kept newest first.
-}
type alias Store =
    { exercises : List Exercise
    , presets : List Preset
    , sessions : List Session
    , live : Maybe LiveSession
    , settings : Settings
    }


version : Int
version =
    1


empty : Store
empty =
    { exercises =
        [ { id = "free"
          , name = "Свободная игра"
          , startBpm = 100
          , subdivision = 1
          , metronome = False
          , notes = "Импровизация, джем, просто поиграть. Тоже идёт в общее время."
          , archived = False
          }
        ]
    , presets = []
    , sessions = []
    , live = Nothing
    , settings = { volume = 0.8 }
    }



-- ENCODE


encode : Store -> E.Value
encode s =
    E.object
        [ ( "version", E.int version )
        , ( "exercises", E.list encodeExercise s.exercises )
        , ( "presets", E.list encodePreset s.presets )
        , ( "sessions", E.list encodeSession s.sessions )
        , ( "live", s.live |> Maybe.map encodeLive |> Maybe.withDefault E.null )
        , ( "settings", E.object [ ( "volume", E.float s.settings.volume ) ] )
        ]


encodeExercise : Exercise -> E.Value
encodeExercise e =
    E.object
        [ ( "id", E.string e.id )
        , ( "name", E.string e.name )
        , ( "startBpm", E.int e.startBpm )
        , ( "subdivision", E.int e.subdivision )
        , ( "metronome", E.bool e.metronome )
        , ( "notes", E.string e.notes )
        , ( "archived", E.bool e.archived )
        ]


encodePreset : Preset -> E.Value
encodePreset p =
    E.object
        [ ( "id", E.string p.id )
        , ( "name", E.string p.name )
        , ( "items"
          , E.list
                (\i -> E.object [ ( "exerciseId", E.string i.exerciseId ), ( "minutes", E.int i.minutes ) ])
                p.items
          )
        ]


encodeSegment : Segment -> E.Value
encodeSegment s =
    E.object
        [ ( "exerciseId", E.string s.exerciseId )
        , ( "startedAt", E.int s.startedAt )
        , ( "durationMs", E.int s.durationMs )
        , ( "startBpm", E.int s.startBpm )
        , ( "maxBpm", E.int s.maxBpm )
        ]


encodeSession : Session -> E.Value
encodeSession s =
    E.object
        [ ( "id", E.string s.id )
        , ( "startedAt", E.int s.startedAt )
        , ( "endedAt", E.int s.endedAt )
        , ( "presetId", maybe E.string s.presetId )
        , ( "segments", E.list encodeSegment s.segments )
        ]


encodeActive : Active -> E.Value
encodeActive a =
    E.object
        [ ( "exerciseId", E.string a.exerciseId )
        , ( "bpm", E.int a.bpm )
        , ( "subdivision", E.int a.subdivision )
        , ( "metronomeOn", E.bool a.metronomeOn )
        , ( "startBpm", E.int a.startBpm )
        , ( "maxBpm", E.int a.maxBpm )
        , ( "startedAt", E.int a.startedAt )
        , ( "accumulatedMs", E.int a.accumulatedMs )
        , ( "runningSince", maybe E.int a.runningSince )
        , ( "bpmMark", E.int a.bpmMark )
        ]


encodeLive : LiveSession -> E.Value
encodeLive l =
    E.object
        [ ( "id", E.string l.id )
        , ( "startedAt", E.int l.startedAt )
        , ( "presetId", maybe E.string l.presetId )
        , ( "segments", E.list encodeSegment l.segments )
        , ( "active", maybe encodeActive l.active )
        , ( "chimed", E.list E.string l.chimed )
        , ( "heartbeat", E.int l.heartbeat )
        ]


maybe : (a -> E.Value) -> Maybe a -> E.Value
maybe f =
    Maybe.map f >> Maybe.withDefault E.null



-- DECODE


decoder : Decoder Store
decoder =
    D.map5 Store
        (D.field "exercises" (D.list exerciseDecoder))
        (D.field "presets" (D.list presetDecoder))
        (D.field "sessions" (D.list sessionDecoder))
        (D.field "live" (D.nullable liveDecoder))
        (D.field "settings" (D.map Settings (D.field "volume" D.float)))


exerciseDecoder : Decoder Exercise
exerciseDecoder =
    D.map7 Exercise
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "startBpm" D.int)
        (D.field "subdivision" D.int)
        (D.field "metronome" D.bool)
        (D.field "notes" D.string)
        (D.field "archived" D.bool)


presetDecoder : Decoder Preset
presetDecoder =
    D.map3 Preset
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "items"
            (D.list
                (D.map2 PresetItem
                    (D.field "exerciseId" D.string)
                    (D.field "minutes" D.int)
                )
            )
        )


segmentDecoder : Decoder Segment
segmentDecoder =
    D.map5 Segment
        (D.field "exerciseId" D.string)
        (D.field "startedAt" D.int)
        (D.field "durationMs" D.int)
        (D.field "startBpm" D.int)
        (D.field "maxBpm" D.int)


sessionDecoder : Decoder Session
sessionDecoder =
    D.map5 Session
        (D.field "id" D.string)
        (D.field "startedAt" D.int)
        (D.field "endedAt" D.int)
        (D.field "presetId" (D.nullable D.string))
        (D.field "segments" (D.list segmentDecoder))


activeDecoder : Decoder Active
activeDecoder =
    D.succeed Active
        |> andField "exerciseId" D.string
        |> andField "bpm" D.int
        |> andField "subdivision" D.int
        |> andField "metronomeOn" D.bool
        |> andField "startBpm" D.int
        |> andField "maxBpm" D.int
        |> andField "startedAt" D.int
        |> andField "accumulatedMs" D.int
        |> andField "runningSince" (D.nullable D.int)
        |> andField "bpmMark" D.int


liveDecoder : Decoder LiveSession
liveDecoder =
    D.map7 LiveSession
        (D.field "id" D.string)
        (D.field "startedAt" D.int)
        (D.field "presetId" (D.nullable D.string))
        (D.field "segments" (D.list segmentDecoder))
        (D.field "active" (D.nullable activeDecoder))
        (D.field "chimed" (D.list D.string))
        (D.field "heartbeat" D.int)


andField : String -> Decoder a -> Decoder (a -> b) -> Decoder b
andField name d =
    D.map2 (|>) (D.field name d)



-- MERGE IMPORT


type alias ImportExercise =
    { name : String
    , bpm : Maybe Int
    , subdivision : Maybe Int
    , metronome : Maybe Bool
    , notes : Maybe String
    }


type alias ImportPreset =
    { name : String
    , items : List ( String, Int )
    }


optional : String -> Decoder a -> Decoder (Maybe a)
optional name d =
    D.maybe (D.field name d)


importDecoder : Decoder ( List ImportExercise, List ImportPreset )
importDecoder =
    D.map2 Tuple.pair
        (optional "exercises"
            (D.list
                (D.map5 ImportExercise
                    (D.field "name" D.string)
                    (optional "bpm" D.int)
                    (optional "subdivision" D.int)
                    (optional "metronome" D.bool)
                    (optional "notes" D.string)
                )
            )
            |> D.map (Maybe.withDefault [])
        )
        (optional "presets"
            (D.list
                (D.map2 ImportPreset
                    (D.field "name" D.string)
                    (D.field "items"
                        (D.list
                            (D.map2 Tuple.pair
                                (D.field "exercise" D.string)
                                (D.field "minutes" D.int)
                            )
                        )
                    )
                )
            )
            |> D.map (Maybe.withDefault [])
        )


sameName : String -> String -> Bool
sameName a b =
    String.toLower (String.trim a) == String.toLower (String.trim b)


{-| Apply pasted JSON. A full backup (has `version`) replaces everything;
anything else is merged: exercises and presets are upserted by name and
history is never touched. `idSeed` keeps generated ids unique.
-}
mergeImport : Int -> String -> Store -> Result String Store
mergeImport idSeed json store =
    case D.decodeString (D.field "version" D.int) json of
        Ok _ ->
            D.decodeString decoder json
                |> Result.mapError (\_ -> "Похоже на резервную копию, но её не удалось прочитать.")

        Err _ ->
            case D.decodeString importDecoder json of
                Err err ->
                    Err ("Не удалось прочитать JSON: " ++ D.errorToString err)

                Ok ( exs, presets ) ->
                    let
                        exercises =
                            List.foldl upsertExercise ( idSeed, store.exercises ) exs |> Tuple.second

                        resolve ( name, minutes ) =
                            exercises
                                |> List.filter (\e -> sameName e.name name)
                                |> List.head
                                |> Maybe.map (\e -> PresetItem e.id minutes)
                                |> Result.fromMaybe name

                        resolved =
                            List.map (\ip -> ( ip.name, List.map resolve ip.items )) presets

                        missing =
                            List.concatMap (Tuple.second >> List.filterMap errorOf) resolved
                    in
                    if List.isEmpty missing then
                        Ok
                            { store
                                | exercises = exercises
                                , presets =
                                    List.foldl
                                        (\( name, items ) ( n, ps ) ->
                                            ( n + 1, upsertPreset n name (Domain.dedupeItems (List.filterMap Result.toMaybe items)) ps )
                                        )
                                        ( idSeed, store.presets )
                                        resolved
                                        |> Tuple.second
                            }

                    else
                        Err ("Неизвестные упражнения в пресетах: " ++ String.join ", " missing)


errorOf : Result e a -> Maybe e
errorOf r =
    case r of
        Err e ->
            Just e

        Ok _ ->
            Nothing


upsertExercise : ImportExercise -> ( Int, List Exercise ) -> ( Int, List Exercise )
upsertExercise ie ( n, exercises ) =
    if List.any (\e -> sameName e.name ie.name) exercises then
        ( n
        , List.map
            (\e ->
                if sameName e.name ie.name then
                    { e
                        | startBpm = Maybe.withDefault e.startBpm ie.bpm |> Domain.clampBpm
                        , subdivision = Maybe.withDefault e.subdivision ie.subdivision |> clamp 1 4
                        , metronome = Maybe.withDefault e.metronome ie.metronome
                        , notes = Maybe.withDefault e.notes ie.notes
                        , archived = False
                    }

                else
                    e
            )
            exercises
        )

    else
        ( n + 1
        , exercises
            ++ [ { id = "e" ++ String.fromInt n
                 , name = String.trim ie.name
                 , startBpm = Maybe.withDefault 80 ie.bpm |> Domain.clampBpm
                 , subdivision = Maybe.withDefault 1 ie.subdivision |> clamp 1 4
                 , metronome = Maybe.withDefault True ie.metronome
                 , notes = Maybe.withDefault "" ie.notes
                 , archived = False
                 }
               ]
        )


upsertPreset : Int -> String -> List PresetItem -> List Preset -> List Preset
upsertPreset n name items presets =
    if List.any (\p -> sameName p.name name) presets then
        List.map
            (\p ->
                if sameName p.name name then
                    { p | items = items }

                else
                    p
            )
            presets

    else
        presets ++ [ { id = "p" ++ String.fromInt n, name = String.trim name, items = items } ]
