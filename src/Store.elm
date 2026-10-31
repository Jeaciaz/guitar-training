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

import Domain exposing (Active, Choice, Exercise, LiveSession, PlanItem, Preset, PresetItem, Segment, Session)
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
    { exercises = library
    , presets =
        [ { id = "daily"
          , name = "Ежедневная"
          , items =
                [ slot [ fixed "legato-workout" 3 ]
                , slot [ random "sweep" 10 ]
                , slot [ random "sequence" 8, random "lick" 7, random "riff" 5 ]
                , slot [ random "song" 12, random "rhythm" 5 ]
                ]
          }
        , { id = "short"
          , name = "Короткая"
          , items =
                [ slot [ fixed "legato-workout" 3 ]
                , slot [ random "sweep" 5 ]
                , slot [ random "sequence" 5, random "lick" 5, random "riff" 4 ]
                , slot [ random "song" 7, random "rhythm" 4 ]
                ]
          }
        ]
    , sessions = []
    , live = Nothing
    , settings = { volume = 0.8 }
    }


slot : List Choice -> PresetItem
slot choices =
    { choices = choices }


fixed : String -> Int -> Choice
fixed id minutes =
    { exerciseId = id, minutes = minutes, random = False }


random : String -> Int -> Choice
random id minutes =
    { exerciseId = id, minutes = minutes, random = True }


{-| The standard library a fresh install starts with: generic exercises
plus variants (linked by `parentId`).
-}
library : List Exercise
library =
    let
        generic id name =
            { id = id
            , name = name
            , startBpm = 60
            , subdivision = 1
            , metronome = True
            , notes = ""
            , archived = False
            , parentId = Nothing
            }

        variant parentId id name bpm =
            { id = id
            , name = name
            , startBpm = bpm
            , subdivision = 1
            , metronome = True
            , notes = ""
            , archived = False
            , parentId = Just parentId
            }
    in
    [ generic "song" "Песня"
    , variant "song" "song-crying-fire" "Avatar — Crying Fire" 88
    , generic "rhythm" "Ритм"
    , variant "rhythm" "rhythm-blues" "Blues" 90
    , generic "lick" "Лик"
    , variant "lick" "lick-blues" "Blues" 90
    , generic "sequence" "Секвенция"
    , variant "sequence" "sequence-slash" "Slash's sequence" 100
    , variant "sequence" "sequence-fours-am" "Fours over A minor" 90
    , variant "sequence" "sequence-fours-ahm" "Fours over A harmonic minor" 90
    , generic "riff" "Рифф"
    , generic "sweep" "Свип-арпеджио"
    , variant "sweep" "sweep-e" "E" 90
    , generic "legato" "Легато"
    , variant "legato" "legato-workout" "Workout" 80
    , { id = "free"
      , name = "Свободная игра"
      , startBpm = 100
      , subdivision = 1
      , metronome = False
      , notes = "Импровизация, джем, просто поиграть. Тоже идёт в общее время."
      , archived = False
      , parentId = Nothing
      }
    ]



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
        , ( "parentId", maybe E.string e.parentId )
        ]


encodePreset : Preset -> E.Value
encodePreset p =
    E.object
        [ ( "id", E.string p.id )
        , ( "name", E.string p.name )
        , ( "items"
          , E.list (\i -> E.object [ ( "choices", E.list encodeChoice i.choices ) ]) p.items
          )
        ]


encodeChoice : Choice -> E.Value
encodeChoice c =
    E.object
        [ ( "exerciseId", E.string c.exerciseId )
        , ( "minutes", E.int c.minutes )
        , ( "random", E.bool c.random )
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
        , ( "plan", E.list encodePlanItem l.plan )
        ]


encodePlanItem : PlanItem -> E.Value
encodePlanItem i =
    E.object
        [ ( "exerciseId", E.string i.exerciseId )
        , ( "minutes", E.int i.minutes )
        , ( "choices", E.list encodeChoice i.choices )
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
    D.succeed Exercise
        |> andField "id" D.string
        |> andField "name" D.string
        |> andField "startBpm" D.int
        |> andField "subdivision" D.int
        |> andField "metronome" D.bool
        |> andField "notes" D.string
        |> andField "archived" D.bool
        -- Absent in data saved before variants existed.
        |> D.map2 (|>) (D.oneOf [ D.field "parentId" (D.nullable D.string), D.succeed Nothing ])


presetDecoder : Decoder Preset
presetDecoder =
    D.map3 Preset
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "items"
            (D.list
                (D.oneOf
                    [ D.map PresetItem (D.field "choices" (D.list choiceDecoder))

                    -- Data saved before slots: one exercise per item.
                    , D.map (\c -> PresetItem [ c ]) choiceDecoder
                    ]
                )
            )
        )


choiceDecoder : Decoder Choice
choiceDecoder =
    D.map3 Choice
        (D.field "exerciseId" D.string)
        (D.field "minutes" D.int)
        (D.oneOf [ D.field "random" D.bool, D.succeed False ])


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
    D.succeed LiveSession
        |> andField "id" D.string
        |> andField "startedAt" D.int
        |> andField "presetId" (D.nullable D.string)
        |> andField "segments" (D.list segmentDecoder)
        |> andField "active" (D.nullable activeDecoder)
        |> andField "chimed" (D.list D.string)
        |> andField "heartbeat" D.int
        -- Absent in sessions saved before plans existed.
        |> D.map2 (|>) (D.oneOf [ D.field "plan" (D.list planItemDecoder), D.succeed [] ])


planItemDecoder : Decoder PlanItem
planItemDecoder =
    D.map3 PlanItem
        (D.field "exerciseId" D.string)
        (D.field "minutes" D.int)
        -- Absent in plans saved by a pre-release build.
        (D.oneOf [ D.field "choices" (D.list choiceDecoder), D.succeed [] ])


andField : String -> Decoder a -> Decoder (a -> b) -> Decoder b
andField name d =
    D.map2 (|>) (D.field name d)



-- MERGE IMPORT


type alias ImportExercise =
    { name : String
    , parent : Maybe String
    , bpm : Maybe Int
    , subdivision : Maybe Int
    , metronome : Maybe Bool
    , notes : Maybe String
    }


{-| Each item is a slot of one or more choices.
-}
type alias ImportPreset =
    { name : String
    , items : List (List ImportChoice)
    }


type alias ImportChoice =
    { exercise : String
    , minutes : Int
    , random : Bool
    }


optional : String -> Decoder a -> Decoder (Maybe a)
optional name d =
    D.maybe (D.field name d)


importDecoder : Decoder ( List ImportExercise, List ImportPreset )
importDecoder =
    D.map2 Tuple.pair
        (optional "exercises"
            (D.list
                (D.map6 ImportExercise
                    (D.field "name" D.string)
                    (optional "parent" D.string)
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
                            (D.oneOf
                                [ D.field "choices" (D.list importChoiceDecoder)
                                , D.map List.singleton importChoiceDecoder
                                ]
                            )
                        )
                    )
                )
            )
            |> D.map (Maybe.withDefault [])
        )


importChoiceDecoder : Decoder ImportChoice
importChoiceDecoder =
    D.map3 ImportChoice
        (D.field "exercise" D.string)
        (D.field "minutes" D.int)
        (optional "random" D.bool |> D.map (Maybe.withDefault False))


sameName : String -> String -> Bool
sameName a b =
    String.toLower (String.trim a) == String.toLower (String.trim b)


{-| Find an exercise by its display name ("Лик · Blues"), or by its plain
name when that is unambiguous.
-}
findByName : List Exercise -> String -> Maybe Exercise
findByName exercises name =
    case List.filter (\e -> sameName (Domain.displayName exercises e) name) exercises of
        e :: _ ->
            Just e

        [] ->
            case List.filter (\e -> sameName e.name name) exercises of
                [ e ] ->
                    Just e

                _ ->
                    Nothing


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
                    List.foldl (\ie acc -> Result.andThen (upsertExercise ie) acc) (Ok ( idSeed, store.exercises )) exs
                        |> Result.andThen
                            (\( _, exercises ) ->
                                let
                                    -- A random choice names the generic it picks from.
                                    resolve item =
                                        (if item.random then
                                            List.filter (\e -> e.parentId == Nothing && sameName e.name item.exercise) exercises
                                                |> List.head

                                         else
                                            findByName exercises item.exercise
                                        )
                                            |> Maybe.map (\e -> Choice e.id item.minutes item.random)
                                            |> Result.fromMaybe item.exercise

                                    resolved =
                                        List.map (\ip -> ( ip.name, List.map (List.map resolve) ip.items )) presets

                                    missing =
                                        List.concatMap (Tuple.second >> List.concat >> List.filterMap errorOf) resolved
                                in
                                if List.isEmpty missing then
                                    Ok
                                        { store
                                            | exercises = exercises
                                            , presets =
                                                List.foldl
                                                    (\( name, items ) ( n, ps ) ->
                                                        ( n + 1, upsertPreset n name (Domain.dedupeItems (List.map (List.filterMap Result.toMaybe >> PresetItem) items)) ps )
                                                    )
                                                    ( idSeed, store.presets )
                                                    resolved
                                                    |> Tuple.second
                                        }

                                else
                                    Err ("Неизвестные упражнения в пресетах: " ++ String.join ", " missing)
                            )


errorOf : Result e a -> Maybe e
errorOf r =
    case r of
        Err e ->
            Just e

        Ok _ ->
            Nothing


upsertExercise : ImportExercise -> ( Int, List Exercise ) -> Result String ( Int, List Exercise )
upsertExercise ie ( n, exercises ) =
    let
        parent =
            case ie.parent of
                Just name ->
                    exercises
                        |> List.filter (\e -> e.parentId == Nothing && sameName e.name name)
                        |> List.head
                        |> Maybe.map (Just << .id)
                        |> Result.fromMaybe ("Неизвестное родительское упражнение: " ++ name)

                Nothing ->
                    Ok Nothing

        isSame parentId e =
            e.parentId == parentId && sameName e.name ie.name
    in
    parent
        |> Result.map
            (\parentId ->
                if List.any (isSame parentId) exercises then
                    ( n
                    , List.map
                        (\e ->
                            if isSame parentId e then
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
                             , startBpm = Maybe.withDefault 60 ie.bpm |> Domain.clampBpm
                             , subdivision = Maybe.withDefault 1 ie.subdivision |> clamp 1 4
                             , metronome = Maybe.withDefault True ie.metronome
                             , notes = Maybe.withDefault "" ie.notes
                             , archived = False
                             , parentId = parentId
                             }
                           ]
                    )
            )


upsertPreset : Int -> String -> List PresetItem -> List Preset -> List Preset
upsertPreset n name items presets =
    if List.any (\pr -> sameName pr.name name) presets then
        List.map
            (\pr ->
                if sameName pr.name name then
                    { pr | items = items }

                else
                    pr
            )
            presets

    else
        presets ++ [ { id = "p" ++ String.fromInt n, name = String.trim name, items = items } ]
