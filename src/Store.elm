module Store exposing
    ( Import(..)
    , Settings
    , Store
    , addExercise
    , allSegments
    , applyImport
    , bump
    , closeIfIdle
    , currentExercise
    , decodeImport
    , decoder
    , deleteExercise
    , empty
    , encode
    , findExercise
    , isRunning
    , legacyDecoder
    , setSubdivision
    , subdivision
    , switchTo
    , tick
    , togglePlay
    , updateExercise
    )

{-| Everything persisted to localStorage, the operations that keep it
consistent, its JSON codec, the migration from the v1 (presets) app, and the
"paste JSON" import.

Invariants kept here:

  - The deck names an existing exercise or free play. `Session.Deck` is
    opaque, so its exercise only changes through `Session.switch` (used by
    `switchTo`), which closes the open segment first.
  - Until something is played on it, the deck follows its exercise's start
    BPM: it is set on `switchTo` and whenever that start BPM changes
    (`followStartBpm`), never on other edits.

-}

import Bpm exposing (Bpm)
import Exercise exposing (Exercise, Subdivision(..))
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Session exposing (Deck, Live, Segment, Session)
import Theme exposing (Theme)


type alias Settings =
    { volume : Float
    , theme : Theme
    }


{-| `sessions` are kept newest first.
-}
type alias Store =
    { exercises : List Exercise
    , sessions : List Session
    , live : Maybe Live
    , deck : Deck
    , settings : Settings
    }


version : Int
version =
    2


empty : Store
empty =
    { exercises = []
    , sessions = []
    , live = Nothing
    , deck = Session.newDeck
    , settings = { volume = 0.8, theme = Theme.Lamp }
    }



-- QUERIES


findExercise : Store -> String -> Maybe Exercise
findExercise store id =
    List.filter (\e -> e.id == id) store.exercises |> List.head


currentExercise : Store -> Maybe Exercise
currentExercise store =
    Session.exerciseId store.deck |> Maybe.andThen (findExercise store)


subdivision : Store -> Subdivision
subdivision store =
    currentExercise store
        |> Maybe.map .subdivision
        |> Maybe.withDefault (Session.freeSubdivision store.deck)


isRunning : Store -> Bool
isRunning store =
    store.live |> Maybe.map Session.isRunning |> Maybe.withDefault False


{-| History plus the session in progress, as of `now`.
-}
allSegments : Int -> Store -> List Segment
allSegments now store =
    List.concatMap .segments store.sessions
        ++ (store.live |> Maybe.map (Session.segmentsAsOf now store.deck) |> Maybe.withDefault [])



-- PLAYING


togglePlay : Int -> Store -> Store
togglePlay now store =
    case store.live of
        Just live ->
            if Session.isRunning live then
                { store | live = Just (Session.pause now store.deck live) }

            else
                { store | live = Just (Session.play now store.deck store.live) }

        Nothing ->
            { store | live = Just (Session.play now store.deck Nothing) }


bump : Int -> Int -> Store -> Store
bump now delta store =
    let
        ( deck, live ) =
            Session.setTempo now (Bpm.add delta (Session.bpm store.deck)) store.deck store.live
    in
    { store | deck = deck, live = live }


{-| Keep a running session's heartbeat fresh; close one paused for 30+
minutes.
-}
tick : Int -> Store -> Store
tick now store =
    closeIfIdle now { store | live = Maybe.map (Session.touch now store.deck) store.live }


closeIfIdle : Int -> Store -> Store
closeIfIdle now store =
    case store.live of
        Just live ->
            if Session.isIdle now live then
                { store
                    | live = Nothing
                    , sessions =
                        -- Paused, so finishing now adds no time.
                        case Session.finish now store.deck live of
                            Just session ->
                                session :: store.sessions

                            Nothing ->
                                store.sessions
                }

            else
                store

        Nothing ->
            store


{-| Put an exercise (or free play) on the deck, paused, at its start BPM.
An unknown id means free play.
-}
switchTo : Int -> Maybe String -> Store -> Store
switchTo now exerciseId store =
    let
        target =
            exerciseId
                |> Maybe.andThen (findExercise store)
                |> Maybe.map (\e -> ( e.id, e.startBpm ))

        ( deck, live ) =
            Session.switch now target store.deck store.live
    in
    { store | deck = deck, live = live }


{-| After an edit (`before` → `after`): if the deck exercise's start BPM
changed, the deck follows it while nothing has been played on it yet.
-}
followStartBpm : Store -> Store -> Store
followStartBpm before after =
    let
        start =
            currentExercise >> Maybe.map .startBpm
    in
    case start after of
        Just bpm ->
            if start before /= Just bpm then
                { after | deck = Session.followStart bpm after.deck after.live }

            else
                after

        Nothing ->
            after



-- EXERCISES


addExercise : Exercise -> Store -> Store
addExercise e store =
    { store | exercises = store.exercises ++ [ e ] }


updateExercise : String -> (Exercise -> Exercise) -> Store -> Store
updateExercise id f store =
    followStartBpm store
        { store
            | exercises =
                List.map
                    (\e ->
                        if e.id == id then
                            f e

                        else
                            e
                    )
                    store.exercises
        }


{-| History keeps its time under the deleted id.
-}
deleteExercise : Int -> String -> Store -> Store
deleteExercise now id store =
    let
        off =
            if Session.exerciseId store.deck == Just id then
                switchTo now Nothing store

            else
                store
    in
    { off | exercises = List.filter (\e -> e.id /= id) off.exercises }


setSubdivision : Subdivision -> Store -> Store
setSubdivision sub store =
    case currentExercise store of
        Just e ->
            updateExercise e.id (\x -> { x | subdivision = sub }) store

        Nothing ->
            { store | deck = Session.setFreeSubdivision sub store.deck }



-- ENCODE


encode : Store -> E.Value
encode store =
    E.object
        [ ( "version", E.int version )
        , ( "exercises", E.list Exercise.encode store.exercises )
        , ( "sessions", E.list Session.encodeSession store.sessions )
        , ( "live", store.live |> Maybe.map Session.encodeLive |> Maybe.withDefault E.null )
        , ( "deck", Session.encodeDeck store.deck )
        , ( "settings"
          , E.object
                [ ( "volume", E.float store.settings.volume )
                , ( "theme", Theme.encode store.settings.theme )
                ]
          )
        ]



-- DECODE (v2)


{-| Out-of-range values are clamped or defaulted; only a wrong structure
fails. A live session is paused at its last heartbeat.
-}
decoder : Decoder Store
decoder =
    D.field "version" D.int
        |> D.andThen
            (\v ->
                if v == version then
                    D.map5 Store
                        (D.field "exercises" (D.list Exercise.decoder))
                        (D.field "sessions" (D.list Session.sessionDecoder))
                        (D.field "live" (D.nullable (D.map Session.restore Session.liveDecoder)))
                        (D.field "deck" Session.deckDecoder)
                        (D.field "settings"
                            (D.map2 Settings
                                (D.field "volume" (D.map (clamp 0 1) D.float))
                                (D.field "theme" Theme.decoder)
                            )
                        )
                        |> D.map dropDanglingDeck

                else
                    D.fail ("Unsupported version " ++ String.fromInt v)
            )


{-| A deck naming a missing exercise falls back to free play. The live
session was just restored paused, so the time given to `switchTo` doesn't
matter.
-}
dropDanglingDeck : Store -> Store
dropDanglingDeck store =
    if Session.exerciseId store.deck /= Nothing && currentExercise store == Nothing then
        switchTo 0 Nothing store

    else
        store



-- MIGRATION FROM v1


{-| Data from the presets app. Only practice time and the click volume carry
over: every segment becomes free play with no top BPM, and exercises and
presets are dropped. Lenient on purpose: anything it can't read is skipped
rather than failing the whole migration.
-}
legacyDecoder : Decoder Store
legacyDecoder =
    D.map3
        (\sessions live volume ->
            { empty
                | sessions =
                    (sessions ++ live)
                        |> List.filter (not << List.isEmpty << .segments)
                        |> List.sortBy (.startedAt >> negate)
                , settings = { volume = clamp 0 1 volume, theme = empty.settings.theme }
            }
        )
        (lenient "sessions" (lenientList legacySession) [])
        (lenient "live" (D.nullable legacyLive |> D.map (Maybe.map List.singleton >> Maybe.withDefault [])) [])
        (lenient "settings" (D.field "volume" D.float) empty.settings.volume)


lenient : String -> Decoder a -> a -> Decoder a
lenient name d fallback =
    D.oneOf [ D.field name d, D.succeed fallback ]


lenientList : Decoder a -> Decoder (List a)
lenientList d =
    D.list (D.oneOf [ D.map Just d, D.succeed Nothing ])
        |> D.map (List.filterMap identity)


freePlay : Int -> Int -> Segment
freePlay startedAt durationMs =
    { exerciseId = Nothing, startedAt = startedAt, durationMs = durationMs, topBpm = Nothing }


legacySegment : Decoder Segment
legacySegment =
    D.map2 freePlay
        (D.field "startedAt" D.int)
        (D.field "durationMs" D.int)


legacySession : Decoder Session
legacySession =
    lenient "segments" (lenientList legacySegment) []
        |> D.andThen sessionAround


{-| A v1 session still in progress: its closed segments plus the time on the
exercise that was on screen, counted up to the last heartbeat.
-}
legacyLive : Decoder Session
legacyLive =
    D.map3
        (\segments active heartbeat ->
            let
                current =
                    case active of
                        Just ( startedAt, accumulated, runningSince ) ->
                            let
                                elapsed =
                                    case ( runningSince, heartbeat ) of
                                        ( Just since, Just hb ) ->
                                            accumulated + max 0 (hb - since)

                                        _ ->
                                            accumulated
                            in
                            if elapsed >= 1000 then
                                [ freePlay startedAt elapsed ]

                            else
                                []

                        Nothing ->
                            []
            in
            segments ++ current
        )
        (lenient "segments" (lenientList legacySegment) [])
        (lenient "active"
            (D.nullable
                (D.map3 (\a b c -> ( a, b, c ))
                    (D.field "startedAt" D.int)
                    (D.field "accumulatedMs" D.int)
                    (lenient "runningSince" (D.nullable D.int) Nothing)
                )
            )
            Nothing
        )
        (D.maybe (D.field "heartbeat" D.int))
        |> D.andThen sessionAround


{-| A session around `segments`, taking id and times from the object when
present.
-}
sessionAround : List Segment -> Decoder Session
sessionAround segments =
    let
        start =
            List.map .startedAt segments |> List.minimum |> Maybe.withDefault 0

        end =
            List.map (\s -> s.startedAt + s.durationMs) segments |> List.maximum |> Maybe.withDefault start
    in
    D.map3 (\id startedAt endedAt -> Session id startedAt endedAt segments)
        (lenient "id" D.string ("s" ++ String.fromInt start))
        (lenient "startedAt" D.int start)
        (lenient "endedAt" D.int end)



-- IMPORT


type alias ImportExercise =
    { name : String
    , bpm : Maybe Bpm
    , subdivision : Maybe Subdivision
    , video : Maybe String
    }


type Import
    = FullBackup Store
    | HistoryOnly (List Session)
    | Exercises (List ImportExercise)


{-| A v2 backup replaces everything; a v1 backup brings its practice time
only; anything else must be `{ "exercises": [...] }` to merge by name.
-}
decodeImport : String -> Result String Import
decodeImport json =
    D.decodeString
        (lenient "version" D.int 0
            |> D.andThen
                (\v ->
                    case v of
                        2 ->
                            D.map FullBackup decoder

                        1 ->
                            D.map (HistoryOnly << .sessions) legacyDecoder

                        _ ->
                            D.map Exercises (D.field "exercises" (D.list importExercise))
                )
        )
        json
        |> Result.mapError D.errorToString


importExercise : Decoder ImportExercise
importExercise =
    D.map4 ImportExercise
        (D.field "name" D.string)
        (D.maybe (D.oneOf [ D.field "bpm" Bpm.decoder, D.field "startBpm" Bpm.decoder ]))
        (D.maybe (D.field "subdivision" Exercise.subdivisionDecoder))
        (D.maybe (D.field "video" D.string))


{-| Apply a decoded import. v1 history is merged by session id, so importing
the same file twice changes nothing.
-}
applyImport : Int -> Import -> Store -> Store
applyImport now imp store =
    case imp of
        FullBackup backup ->
            backup

        HistoryOnly sessions ->
            let
                known =
                    List.map .id store.sessions
            in
            { store
                | sessions =
                    (store.sessions ++ List.filter (\s -> not (List.member s.id known)) sessions)
                        |> List.sortBy (.startedAt >> negate)
            }

        Exercises list ->
            followStartBpm store { store | exercises = mergeExercises now list store.exercises }


{-| Upsert by name (case-insensitive). New ids are derived from `now`. An
empty `video` clears it.
-}
mergeExercises : Int -> List ImportExercise -> List Exercise -> List Exercise
mergeExercises now incoming existing =
    List.foldl
        (\( i, imp ) acc ->
            let
                name =
                    String.trim imp.name

                same e =
                    String.toLower e.name == String.toLower name

                video =
                    imp.video
                        |> Maybe.map
                            (\v ->
                                if String.trim v == "" then
                                    Nothing

                                else
                                    Just (String.trim v)
                            )
            in
            if name == "" then
                acc

            else if List.any same acc then
                List.map
                    (\e ->
                        if same e then
                            { e
                                | startBpm = Maybe.withDefault e.startBpm imp.bpm
                                , subdivision = Maybe.withDefault e.subdivision imp.subdivision
                                , video = Maybe.withDefault e.video video
                            }

                        else
                            e
                    )
                    acc

            else
                acc
                    ++ [ { id = "e" ++ String.fromInt now ++ "-" ++ String.fromInt i
                         , name = name
                         , startBpm = Maybe.withDefault (Bpm.fromInt 80) imp.bpm
                         , subdivision = Maybe.withDefault Quarter imp.subdivision
                         , video = Maybe.withDefault Nothing video
                         }
                       ]
        )
        existing
        (List.indexedMap Tuple.pair incoming)
