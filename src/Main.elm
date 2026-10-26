port module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Dict
import Domain exposing (Active, Exercise, LiveSession, Preset, Session)
import File exposing (File)
import File.Download
import File.Select
import Html exposing (Html, a, button, details, div, h1, h2, input, label, main_, nav, option, p, pre, select, span, summary, text, textarea)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, href, placeholder, rows, style, title, type_, value)
import Html.Events exposing (on, onCheck, onClick, onInput, preventDefaultOn)
import Json.Decode as D
import Json.Encode as E
import Store exposing (Store)
import Svg
import Svg.Attributes as SA
import Task
import Time
import Url exposing (Url)



-- PORTS


port saveState : E.Value -> Cmd msg


port metronome : E.Value -> Cmd msg


port wakeLock : Bool -> Cmd msg


port chime : Float -> Cmd msg



-- MODEL


type Route
    = Home
    | SessionRoute
    | Summary String
    | Library
    | EditExercise (Maybe String)
    | EditPreset (Maybe String)
    | Stats
    | Settings


type alias ExerciseForm =
    { id : Maybe String
    , name : String
    , bpm : String
    , subdivision : Int
    , metronome : Bool
    , notes : String
    }


type alias PresetForm =
    { id : Maybe String
    , name : String
    , items : List { exerciseId : String, minutes : String }
    }


type Form
    = NoForm
    | ExForm ExerciseForm
    | PrForm PresetForm


type alias Model =
    { key : Nav.Key
    , route : Route
    , store : Store
    , now : Int
    , zone : Time.Zone
    , form : Form
    , picker : Bool
    , drag : Maybe Drag
    , confirmFinish : Bool
    , confirmDelete : Bool
    , importText : String
    , notice : Maybe ( Bool, String )
    }


{-| A preset item being dragged by its handle. `pitch` is the row height plus
the list gap, so the drop index follows from how far the pointer moved.
-}
type alias Drag =
    { from : Int
    , startY : Float
    , y : Float
    , pitch : Float
    }


type alias Flags =
    { stored : Maybe String
    , now : Int
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        loaded =
            flags.stored
                |> Maybe.andThen (D.decodeString Store.decoder >> Result.toMaybe)
                |> Maybe.withDefault Store.empty

        -- Data saved before the app was translated has the English name.
        renameFreePlay e =
            if e.id == "free" && e.name == "Free play" then
                { e | name = "Свободная игра", notes = "Импровизация, джем, просто поиграть. Тоже идёт в общее время." }

            else
                e

        restored =
            { loaded
                | live = Maybe.map Domain.restore loaded.live
                , exercises = List.map renameFreePlay loaded.exercises
            }

        base =
            { key = key
            , route = parseRoute url
            , store = restored
            , now = flags.now
            , zone = Time.utc
            , form = NoForm
            , picker = False
            , drag = Nothing
            , confirmFinish = False
            , confirmDelete = False
            , importText = ""
            , notice = Nothing
            }

        ( model, cmd ) =
            case restored.live of
                Just live ->
                    if Domain.isIdle flags.now live then
                        finishLive flags.now base

                    else
                        ( base, Cmd.none )

                Nothing ->
                    ( base, Cmd.none )
    in
    ( enterRoute model
    , Cmd.batch
        [ cmd
        , Task.perform GotZone Time.here
        , wakeLock (model.store.live /= Nothing)
        , saveState (Store.encode model.store)
        ]
    )



-- ROUTING


parseRoute : Url -> Route
parseRoute url =
    case String.split "/" (Maybe.withDefault "" url.fragment) |> List.filter ((/=) "") of
        [ "session" ] ->
            SessionRoute

        [ "summary", id ] ->
            Summary id

        [ "library" ] ->
            Library

        [ "exercise", "new" ] ->
            EditExercise Nothing

        [ "exercise", id ] ->
            EditExercise (Just id)

        [ "preset", "new" ] ->
            EditPreset Nothing

        [ "preset", id ] ->
            EditPreset (Just id)

        [ "stats" ] ->
            Stats

        [ "settings" ] ->
            Settings

        _ ->
            Home


routeHref : Route -> String
routeHref route =
    "#/"
        ++ (case route of
                Home ->
                    ""

                SessionRoute ->
                    "session"

                Summary id ->
                    "summary/" ++ id

                Library ->
                    "library"

                EditExercise Nothing ->
                    "exercise/new"

                EditExercise (Just id) ->
                    "exercise/" ++ id

                EditPreset Nothing ->
                    "preset/new"

                EditPreset (Just id) ->
                    "preset/" ++ id

                Stats ->
                    "stats"

                Settings ->
                    "settings"
           )


go : Model -> Route -> Cmd Msg
go model route =
    Nav.pushUrl model.key (routeHref route)


{-| Set up per-page state when a route is entered.
-}
enterRoute : Model -> Model
enterRoute model =
    let
        reset =
            { model | picker = False, drag = Nothing, confirmFinish = False, confirmDelete = False, notice = Nothing }
    in
    case model.route of
        EditExercise id ->
            let
                existing =
                    id |> Maybe.andThen (findExercise model.store)
            in
            { reset
                | form =
                    ExForm
                        { id = Maybe.map .id existing
                        , name = existing |> Maybe.map .name |> Maybe.withDefault ""
                        , bpm = existing |> Maybe.map (.startBpm >> String.fromInt) |> Maybe.withDefault "80"
                        , subdivision = existing |> Maybe.map .subdivision |> Maybe.withDefault 1
                        , metronome = existing |> Maybe.map .metronome |> Maybe.withDefault True
                        , notes = existing |> Maybe.map .notes |> Maybe.withDefault ""
                        }
            }

        EditPreset id ->
            let
                existing =
                    id |> Maybe.andThen (findPreset model.store)
            in
            { reset
                | form =
                    PrForm
                        { id = Maybe.map .id existing
                        , name = existing |> Maybe.map .name |> Maybe.withDefault ""
                        , items =
                            existing
                                |> Maybe.map .items
                                |> Maybe.withDefault []
                                |> List.map (\i -> { exerciseId = i.exerciseId, minutes = String.fromInt i.minutes })
                        }
            }

        SessionRoute ->
            { reset | picker = Maybe.andThen .active model.store.live == Nothing }

        _ ->
            { reset | form = NoForm }



-- UPDATE


{-| Session actions are stamped with a fresh `Time.now` so a throttled tick
never skews the time accounting.
-}
type Action
    = StartPreset String
    | StartQuick
    | SelectExercise String
    | NextExercise
    | TogglePlay
    | BumpBpm Int
    | SetSubdivision Int
    | ToggleClick
    | FinishSession
    | Tick


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | GotZone Time.Zone
    | Act Action
    | Do Action Time.Posix
    | TogglePicker
    | AskFinish Bool
    | AskDelete Bool
    | DiscardSession
    | PromoteBpm String Int
    | DeleteSession String
    | ExName String
    | ExBpm String
    | ExSubdivision Int
    | ExMetronome Bool
    | ExNotes String
    | SaveExercise
    | DeleteExercise String
    | RestoreExercise String
    | PrName String
    | PrItemExercise Int String
    | PrItemMinutes Int String
    | DragStart Int Float Float
    | DragMove Float
    | DragEnd
    | PrItemRemove Int
    | PrAddItem String
    | SavePreset
    | DeletePreset String
    | SetVolume Float
    | Export
    | ImportFile
    | GotFile File
    | GotFileText String
    | ImportTextChanged String
    | ApplyImport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( next, cmd ) =
            updateModel msg model
    in
    ( next, Cmd.batch [ cmd, effects model next ] )


{-| Sync the outside world (storage, metronome, wake lock) with the model.
-}
effects : Model -> Model -> Cmd Msg
effects old new =
    Cmd.batch
        [ if old.store /= new.store then
            saveState (Store.encode new.store)

          else
            Cmd.none
        , if metronomeState old /= metronomeState new then
            metronome (encodeMetronome (metronomeState new))

          else
            Cmd.none
        , if (old.store.live == Nothing) /= (new.store.live == Nothing) then
            wakeLock (new.store.live /= Nothing)

          else
            Cmd.none
        ]


type alias Click =
    { bpm : Int, subdivision : Int, volume : Float }


metronomeState : Model -> Maybe Click
metronomeState model =
    model.store.live
        |> Maybe.andThen .active
        |> Maybe.andThen
            (\a ->
                if a.runningSince /= Nothing && a.metronomeOn then
                    Just { bpm = a.bpm, subdivision = a.subdivision, volume = model.store.settings.volume }

                else
                    Nothing
            )


encodeMetronome : Maybe Click -> E.Value
encodeMetronome state =
    case state of
        Just c ->
            E.object
                [ ( "running", E.bool True )
                , ( "bpm", E.int c.bpm )
                , ( "subdivision", E.int c.subdivision )
                , ( "volume", E.float c.volume )
                ]

        Nothing ->
            E.object [ ( "running", E.bool False ) ]


updateModel : Msg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
    case msg of
        UrlChanged url ->
            ( enterRoute { model | route = parseRoute url }, Cmd.none )

        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        GotZone zone ->
            ( { model | zone = zone }, Cmd.none )

        Act action ->
            ( model, Task.perform (Do action) Time.now )

        Do action posix ->
            act action (Time.posixToMillis posix) { model | now = Time.posixToMillis posix }

        TogglePicker ->
            ( { model | picker = not model.picker }, Cmd.none )

        AskFinish b ->
            ( { model | confirmFinish = b }, Cmd.none )

        AskDelete b ->
            ( { model | confirmDelete = b }, Cmd.none )

        DiscardSession ->
            ( mapStore (\s -> { s | live = Nothing }) { model | confirmFinish = False }, go model Home )

        PromoteBpm exId bpm ->
            ( mapStore
                (\s -> { s | exercises = updateExercise exId (\e -> { e | startBpm = bpm }) s.exercises })
                model
            , Cmd.none
            )

        DeleteSession id ->
            ( mapStore (\s -> { s | sessions = List.filter (\x -> x.id /= id) s.sessions }) model
            , Nav.back model.key 1
            )

        ExName v ->
            ( mapExForm (\f -> { f | name = v }) model, Cmd.none )

        ExBpm v ->
            ( mapExForm (\f -> { f | bpm = v }) model, Cmd.none )

        ExSubdivision v ->
            ( mapExForm (\f -> { f | subdivision = v }) model, Cmd.none )

        ExMetronome v ->
            ( mapExForm (\f -> { f | metronome = v }) model, Cmd.none )

        ExNotes v ->
            ( mapExForm (\f -> { f | notes = v }) model, Cmd.none )

        SaveExercise ->
            case model.form of
                ExForm f ->
                    case ( String.trim f.name, String.toInt f.bpm ) of
                        ( "", _ ) ->
                            ( { model | notice = Just ( False, "Введите название." ) }, Cmd.none )

                        ( _, Nothing ) ->
                            ( { model | notice = Just ( False, "BPM должен быть числом." ) }, Cmd.none )

                        ( name, Just bpm ) ->
                            let
                                apply e =
                                    { e
                                        | name = name
                                        , startBpm = Domain.clampBpm bpm
                                        , subdivision = f.subdivision
                                        , metronome = f.metronome
                                        , notes = String.trim f.notes
                                    }

                                store =
                                    model.store

                                exercises =
                                    case f.id of
                                        Just id ->
                                            updateExercise id apply store.exercises

                                        Nothing ->
                                            store.exercises
                                                ++ [ apply
                                                        { id = freshId "e" model
                                                        , name = ""
                                                        , startBpm = 0
                                                        , subdivision = 1
                                                        , metronome = True
                                                        , notes = ""
                                                        , archived = False
                                                        }
                                                   ]
                            in
                            ( { model | store = { store | exercises = exercises } }, Nav.back model.key 1 )

                _ ->
                    ( model, Cmd.none )

        DeleteExercise id ->
            let
                used =
                    List.any (.segments >> List.any (\seg -> seg.exerciseId == id)) model.store.sessions

                dropFromPresets =
                    List.map (\p -> { p | items = List.filter (\i -> i.exerciseId /= id) p.items })
            in
            ( mapStore
                (\s ->
                    { s
                        | exercises =
                            if used then
                                updateExercise id (\e -> { e | archived = True }) s.exercises

                            else
                                List.filter (\e -> e.id /= id) s.exercises
                        , presets = dropFromPresets s.presets
                    }
                )
                model
            , Nav.back model.key 1
            )

        RestoreExercise id ->
            ( mapStore (\s -> { s | exercises = updateExercise id (\e -> { e | archived = False }) s.exercises }) model
            , Cmd.none
            )

        PrName v ->
            ( mapPrForm (\f -> { f | name = v }) model, Cmd.none )

        PrItemExercise i v ->
            ( mapPrForm (\f -> { f | items = updateAt i (\it -> { it | exerciseId = v }) f.items }) model, Cmd.none )

        PrItemMinutes i v ->
            ( mapPrForm (\f -> { f | items = updateAt i (\it -> { it | minutes = v }) f.items }) model, Cmd.none )

        DragStart i y pitch ->
            ( { model | drag = Just { from = i, startY = y, y = y, pitch = pitch } }, Cmd.none )

        DragMove y ->
            ( { model | drag = Maybe.map (\d -> { d | y = y }) model.drag }, Cmd.none )

        DragEnd ->
            case ( model.drag, model.form ) of
                ( Just d, PrForm f ) ->
                    ( mapPrForm (\form -> { form | items = move d.from (dropIndex (List.length f.items) d) form.items })
                        { model | drag = Nothing }
                    , Cmd.none
                    )

                _ ->
                    ( { model | drag = Nothing }, Cmd.none )

        PrItemRemove i ->
            ( mapPrForm (\f -> { f | items = List.take i f.items ++ List.drop (i + 1) f.items }) model, Cmd.none )

        PrAddItem exId ->
            ( mapPrForm (\f -> { f | items = f.items ++ [ { exerciseId = exId, minutes = "5" } ] }) { model | picker = False }
            , Cmd.none
            )

        SavePreset ->
            case model.form of
                PrForm f ->
                    let
                        items =
                            f.items
                                |> List.map
                                    (\it ->
                                        { exerciseId = it.exerciseId
                                        , minutes = String.toInt it.minutes |> Maybe.withDefault 5 |> clamp 1 600
                                        }
                                    )
                    in
                    if String.trim f.name == "" then
                        ( { model | notice = Just ( False, "Введите название." ) }, Cmd.none )

                    else if List.isEmpty items then
                        ( { model | notice = Just ( False, "Добавьте хотя бы одно упражнение." ) }, Cmd.none )

                    else
                        let
                            preset =
                                { id = Maybe.withDefault (freshId "p" model) f.id
                                , name = String.trim f.name
                                , items = items
                                }
                        in
                        ( mapStore
                            (\s ->
                                { s
                                    | presets =
                                        if f.id == Nothing then
                                            s.presets ++ [ preset ]

                                        else
                                            List.map
                                                (\p ->
                                                    if p.id == preset.id then
                                                        preset

                                                    else
                                                        p
                                                )
                                                s.presets
                                }
                            )
                            model
                        , Nav.back model.key 1
                        )

                _ ->
                    ( model, Cmd.none )

        DeletePreset id ->
            ( mapStore (\s -> { s | presets = List.filter (\p -> p.id /= id) s.presets }) model
            , Nav.back model.key 1
            )

        SetVolume v ->
            ( mapStore (\s -> { s | settings = { volume = v } }) model, Cmd.none )

        Export ->
            ( model
            , File.Download.string
                ("guitar-training-" ++ String.fromInt (Domain.dayKey model.zone model.now) ++ ".json")
                "application/json"
                (E.encode 2 (Store.encode model.store))
            )

        ImportFile ->
            ( model, File.Select.file [ "application/json", ".json" ] GotFile )

        GotFile file ->
            ( model, Task.perform GotFileText (File.toString file) )

        GotFileText text ->
            applyImport text model

        ImportTextChanged v ->
            ( { model | importText = v }, Cmd.none )

        ApplyImport ->
            applyImport model.importText model


applyImport : String -> Model -> ( Model, Cmd Msg )
applyImport text model =
    case Store.mergeImport model.now text model.store of
        Ok store ->
            ( { model
                | store = { store | live = Maybe.map Domain.restore store.live }
                , importText = ""
                , notice = Just ( True, "Импортировано." )
              }
            , Cmd.none
            )

        Err err ->
            ( { model | notice = Just ( False, err ) }, Cmd.none )


act : Action -> Int -> Model -> ( Model, Cmd Msg )
act action now model =
    case action of
        Tick ->
            tick now model

        StartPreset presetId ->
            let
                first =
                    findPreset model.store presetId
                        |> Maybe.andThen (.items >> List.head)
                        |> Maybe.andThen (.exerciseId >> findExercise model.store)

                live =
                    Domain.newLive now (Just presetId)
            in
            ( mapStore
                (\s ->
                    { s
                        | live =
                            Just
                                (case first of
                                    Just ex ->
                                        Domain.startExercise now ex live

                                    Nothing ->
                                        live
                                )
                    }
                )
                model
            , go model SessionRoute
            )

        StartQuick ->
            ( mapStore (\s -> { s | live = Just (Domain.newLive now Nothing) }) model
            , go model SessionRoute
            )

        SelectExercise exId ->
            case findExercise model.store exId of
                Just ex ->
                    ( mapLive (Domain.startExercise now ex) { model | picker = False }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NextExercise ->
            case model.store.live |> Maybe.andThen (nextInPreset now model.store) of
                Just exId ->
                    act (SelectExercise exId) now model

                Nothing ->
                    ( model, Cmd.none )

        TogglePlay ->
            ( mapActive
                (\a ->
                    if a.runningSince == Nothing then
                        Domain.resume now a

                    else
                        Domain.pause now a
                )
                model
                |> mapLive (\l -> { l | heartbeat = now })
            , Cmd.none
            )

        BumpBpm delta ->
            ( mapActive (\a -> Domain.setBpm now (a.bpm + delta) a) model, Cmd.none )

        SetSubdivision n ->
            ( mapActive (\a -> { a | subdivision = n }) model, Cmd.none )

        ToggleClick ->
            ( mapActive (\a -> { a | metronomeOn = not a.metronomeOn }) model, Cmd.none )

        FinishSession ->
            finishLive now model


tick : Int -> Model -> ( Model, Cmd Msg )
tick now model =
    case model.store.live of
        Nothing ->
            ( model, Cmd.none )

        Just live ->
            if Domain.isIdle now live then
                finishLive now model

            else
                let
                    touched =
                        if now - live.heartbeat >= 5000 then
                            Domain.touch now live

                        else
                            live

                    -- The active exercise, if it just reached its preset target.
                    due =
                        case ( touched.active, presetTarget model.store touched ) of
                            ( Just a, Just minutes ) ->
                                if
                                    not (List.member a.exerciseId touched.chimed)
                                        && (Domain.exerciseTimeInLive now a.exerciseId touched >= minutes * 60000)
                                then
                                    Just a.exerciseId

                                else
                                    Nothing

                            _ ->
                                Nothing
                in
                case due of
                    Just exerciseId ->
                        ( mapStore (\s -> { s | live = Just { touched | chimed = exerciseId :: touched.chimed } }) model
                        , chime model.store.settings.volume
                        )

                    Nothing ->
                        ( mapStore (\s -> { s | live = Just touched }) model, Cmd.none )


finishLive : Int -> Model -> ( Model, Cmd Msg )
finishLive now model =
    case model.store.live |> Maybe.andThen (Domain.finish now) of
        Just session ->
            ( mapStore (\s -> { s | live = Nothing, sessions = session :: s.sessions }) { model | confirmFinish = False }
            , go model (Summary session.id)
            )

        Nothing ->
            ( mapStore (\s -> { s | live = Nothing }) { model | confirmFinish = False }, go model Home )


{-| Target minutes for the active exercise in the session's preset.
-}
presetTarget : Store -> LiveSession -> Maybe Int
presetTarget store live =
    Maybe.map2
        (\preset a ->
            preset.items
                |> List.filter (\i -> i.exerciseId == a.exerciseId)
                |> List.map .minutes
                |> List.sum
        )
        (live.presetId |> Maybe.andThen (findPreset store))
        live.active
        |> Maybe.andThen
            (\m ->
                if m > 0 then
                    Just m

                else
                    Nothing
            )


{-| The next unfinished preset item after the current one (wrapping), or
simply the next item when everything is done.
-}
nextInPreset : Int -> Store -> LiveSession -> Maybe String
nextInPreset now store live =
    live.presetId
        |> Maybe.andThen (findPreset store)
        |> Maybe.andThen
            (\preset ->
                let
                    ids =
                        List.map .exerciseId preset.items

                    current =
                        live.active |> Maybe.map .exerciseId

                    idx =
                        ids
                            |> List.indexedMap Tuple.pair
                            |> List.filter (\( _, id ) -> Just id == current)
                            |> List.head
                            |> Maybe.map Tuple.first
                            |> Maybe.withDefault -1

                    rotated =
                        List.drop (idx + 1) preset.items ++ List.take (idx + 1) preset.items

                    candidates =
                        List.filter (\i -> Just i.exerciseId /= current) rotated

                    unfinished =
                        List.filter (\i -> Domain.exerciseTimeInLive now i.exerciseId live < i.minutes * 60000) candidates
                in
                List.head (unfinished ++ candidates) |> Maybe.map .exerciseId
            )



-- HELPERS


mapStore : (Store -> Store) -> Model -> Model
mapStore f model =
    { model | store = f model.store }


mapLive : (LiveSession -> LiveSession) -> Model -> Model
mapLive f =
    mapStore (\s -> { s | live = Maybe.map f s.live })


mapActive : (Active -> Active) -> Model -> Model
mapActive f =
    mapLive (\l -> { l | active = Maybe.map f l.active })


mapExForm : (ExerciseForm -> ExerciseForm) -> Model -> Model
mapExForm f model =
    case model.form of
        ExForm form ->
            { model | form = ExForm (f form), notice = Nothing }

        _ ->
            model


mapPrForm : (PresetForm -> PresetForm) -> Model -> Model
mapPrForm f model =
    case model.form of
        PrForm form ->
            { model | form = PrForm (f form), notice = Nothing }

        _ ->
            model


{-| A time-based id not yet used by any exercise or preset (`now` can lag
by a tick, so two quick saves could otherwise collide).
-}
freshId : String -> Model -> String
freshId prefix model =
    let
        taken =
            List.map .id model.store.exercises ++ List.map .id model.store.presets

        try n =
            let
                id =
                    prefix ++ String.fromInt (model.now + n)
            in
            if List.member id taken then
                try (n + 1)

            else
                id
    in
    try 0


findExercise : Store -> String -> Maybe Exercise
findExercise store id =
    List.filter (\e -> e.id == id) store.exercises |> List.head


findPreset : Store -> String -> Maybe Preset
findPreset store id =
    List.filter (\p -> p.id == id) store.presets |> List.head


exerciseName : Store -> String -> String
exerciseName store id =
    findExercise store id |> Maybe.map .name |> Maybe.withDefault "(удалено)"


updateExercise : String -> (Exercise -> Exercise) -> List Exercise -> List Exercise
updateExercise id f =
    List.map
        (\e ->
            if e.id == id then
                f e

            else
                e
        )


updateAt : Int -> (a -> a) -> List a -> List a
updateAt i f =
    List.indexedMap
        (\j x ->
            if i == j then
                f x

            else
                x
        )


dropIndex : Int -> Drag -> Int
dropIndex count d =
    clamp 0 (count - 1) (d.from + round ((d.y - d.startY) / d.pitch))


move : Int -> Int -> List a -> List a
move from to xs =
    if to < 0 || to >= List.length xs then
        xs

    else
        case List.drop from xs |> List.head of
            Just x ->
                let
                    without =
                        List.take from xs ++ List.drop (from + 1) xs
                in
                List.take to without ++ [ x ] ++ List.drop to without

            Nothing ->
                xs


pad2 : Int -> String
pad2 n =
    String.padLeft 2 '0' (String.fromInt n)


{-| m:ss, or h:mm:ss past an hour.
-}
clock : Int -> String
clock ms =
    let
        total =
            ms // 1000

        h =
            total // 3600

        m =
            modBy 60 (total // 60)

        s =
            modBy 60 total
    in
    if h > 0 then
        String.fromInt h ++ ":" ++ pad2 m ++ ":" ++ pad2 s

    else
        String.fromInt m ++ ":" ++ pad2 s


{-| "1 ч 05 мин" / "23 мин".
-}
duration : Int -> String
duration ms =
    let
        mins =
            ms // 60000
    in
    if mins >= 60 then
        String.fromInt (mins // 60) ++ " ч " ++ pad2 (modBy 60 mins) ++ " мин"

    else
        String.fromInt mins ++ " мин"


{-| Russian plural form for `n`: one (1, 21), few (2–4, 22–24), many.
-}
plural : Int -> String -> String -> String -> String
plural n one few many =
    let
        mod10 =
            modBy 10 n

        mod100 =
            modBy 100 n
    in
    if mod10 == 1 && mod100 /= 11 then
        one

    else if mod10 >= 2 && mod10 <= 4 && (mod100 < 12 || mod100 > 14) then
        few

    else
        many


dateLabel : Time.Zone -> Int -> String
dateLabel zone millis =
    let
        t =
            Time.millisToPosix millis

        weekday =
            case Time.toWeekday zone t of
                Time.Mon ->
                    "Пн"

                Time.Tue ->
                    "Вт"

                Time.Wed ->
                    "Ср"

                Time.Thu ->
                    "Чт"

                Time.Fri ->
                    "Пт"

                Time.Sat ->
                    "Сб"

                Time.Sun ->
                    "Вс"

        month =
            case Time.toMonth zone t of
                Time.Jan ->
                    "янв"

                Time.Feb ->
                    "фев"

                Time.Mar ->
                    "мар"

                Time.Apr ->
                    "апр"

                Time.May ->
                    "мая"

                Time.Jun ->
                    "июн"

                Time.Jul ->
                    "июл"

                Time.Aug ->
                    "авг"

                Time.Sep ->
                    "сен"

                Time.Oct ->
                    "окт"

                Time.Nov ->
                    "ноя"

                Time.Dec ->
                    "дек"
    in
    weekday
        ++ " "
        ++ String.fromInt (Time.toDay zone t)
        ++ " "
        ++ month
        ++ ", "
        ++ pad2 (Time.toHour zone t)
        ++ ":"
        ++ pad2 (Time.toMinute zone t)


subdivisionLabel : Int -> String
subdivisionLabel n =
    case n of
        2 ->
            "восьмые"

        3 ->
            "триоли"

        4 ->
            "шестнадцатые"

        _ ->
            "четверти"


subdivisionIcon : Int -> String
subdivisionIcon n =
    case n of
        2 ->
            "♫"

        3 ->
            "3"

        4 ->
            "♬"

        _ ->
            "♩"



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Гитара: тренировки"
    , body =
        [ div [ class "app" ]
            [ main_ [ class "page" ] (viewPage model)
            , viewNav model
            ]
        ]
    }


viewNav : Model -> Html Msg
viewNav model =
    let
        item route label icon =
            a
                [ href (routeHref route)
                , classList [ ( "active", sameSection model.route route ) ]
                ]
                [ span [ class "nav-icon" ] [ text icon ], span [] [ text label ] ]

        practice =
            if model.store.live /= Nothing then
                item SessionRoute "Сессия" "●"

            else
                item Home "Тренировка" "▶"
    in
    nav [ class "nav" ]
        [ practice
        , item Library "Библиотека" "☰"
        , item Stats "Статистика" "▤"
        , item Settings "Настройки" "⚙"
        ]


sameSection : Route -> Route -> Bool
sameSection current target =
    case ( current, target ) of
        ( EditExercise _, Library ) ->
            True

        ( EditPreset _, Library ) ->
            True

        ( Summary _, Stats ) ->
            True

        ( SessionRoute, Home ) ->
            True

        ( Home, SessionRoute ) ->
            True

        _ ->
            current == target


viewPage : Model -> List (Html Msg)
viewPage model =
    case model.route of
        Home ->
            viewHome model

        SessionRoute ->
            case model.store.live of
                Just live ->
                    viewSession model live

                Nothing ->
                    viewHome model

        Summary id ->
            case List.filter (\s -> s.id == id) model.store.sessions |> List.head of
                Just session ->
                    viewSummary model session

                Nothing ->
                    [ h1 [] [ text "Сессия не найдена" ], a [ class "btn", href (routeHref Home) ] [ text "На главную" ] ]

        Library ->
            viewLibrary model

        EditExercise _ ->
            case model.form of
                ExForm f ->
                    viewExerciseForm model f

                _ ->
                    []

        EditPreset _ ->
            case model.form of
                PrForm f ->
                    viewPresetForm model f

                _ ->
                    []

        Stats ->
            viewStats model

        Settings ->
            viewSettings model



-- VIEW: HOME


viewHome : Model -> List (Html Msg)
viewHome model =
    let
        store =
            model.store

        byDay =
            Domain.minutesByDay model.zone store.sessions

        today =
            Dict.get (Domain.dayKey model.zone model.now) byDay |> Maybe.withDefault 0

        week =
            lastDays model 7 |> List.map Tuple.second |> List.sum

        streak =
            Domain.streak model.zone model.now store.sessions
    in
    [ h1 [] [ text "Тренировка" ]
    , div [ class "tiles" ]
        [ tile "Сегодня" (duration today)
        , tile "7 дней" (duration week)
        , tile "Серия" (String.fromInt streak ++ " " ++ plural streak "день" "дня" "дней")
        ]
    , case store.live of
        Just live ->
            a [ class "card resume", href (routeHref SessionRoute) ]
                [ div [ class "card-title" ] [ text "Продолжить сессию" ]
                , div [ class "muted" ] [ text (clock (Domain.liveTotal model.now live) ++ " практики") ]
                ]

        Nothing ->
            div [ class "stack" ]
                [ h2 [] [ text "Пресеты" ]
                , if List.isEmpty store.presets then
                    p [ class "muted" ]
                        [ text "Пресетов пока нет. "
                        , a [ href (routeHref (EditPreset Nothing)) ] [ text "Создать" ]
                        , text "."
                        ]

                  else
                    div [ class "list" ]
                        (List.map
                            (\preset ->
                                button [ class "card preset", onClick (Act (StartPreset preset.id)) ]
                                    [ div [ class "card-title" ] [ text preset.name ]
                                    , div [ class "muted" ]
                                        [ text
                                            (String.fromInt (List.sum (List.map .minutes preset.items))
                                                ++ " мин · "
                                                ++ String.join ", " (List.map (.exerciseId >> exerciseName store) preset.items)
                                            )
                                        ]
                                    ]
                            )
                            store.presets
                        )
                , button [ class "btn big secondary", onClick (Act StartQuick) ] [ text "Быстрая сессия" ]
                ]
    ]


tile : String -> String -> Html msg
tile label value =
    div [ class "tile" ] [ div [ class "tile-value" ] [ text value ], div [ class "tile-label" ] [ text label ] ]



-- VIEW: SESSION


viewSession : Model -> LiveSession -> List (Html Msg)
viewSession model live =
    let
        store =
            model.store

        preset =
            live.presetId |> Maybe.andThen (findPreset store)
    in
    [ div [ class "session-head" ]
        [ div []
            [ div [ class "muted small" ] [ text (preset |> Maybe.map .name |> Maybe.withDefault "Быстрая сессия") ]
            , div [ class "session-total" ] [ text (clock (Domain.liveTotal model.now live)) ]
            ]
        , button [ class "btn secondary", onClick (AskFinish True) ] [ text "Завершить" ]
        ]
    , if model.confirmFinish then
        div [ class "card confirm" ]
            [ p [] [ text "Завершить сессию?" ]
            , div [ class "row" ]
                [ button [ class "btn", onClick (Act FinishSession) ] [ text "Сохранить и завершить" ]
                , button [ class "btn secondary", onClick (AskFinish False) ] [ text "Продолжить" ]
                ]
            , button [ class "link danger", onClick DiscardSession ] [ text "Удалить сессию" ]
            ]

      else
        text ""
    , case live.active of
        Just a ->
            viewActive model live a

        Nothing ->
            p [ class "muted" ] [ text "Выберите упражнение." ]
    , case preset of
        Just pr ->
            viewQueue model live pr

        Nothing ->
            text ""
    , button [ class "btn secondary wide", onClick TogglePicker ]
        [ text
            (if model.picker then
                "Скрыть упражнения"

             else
                "Сменить упражнение…"
            )
        ]
    , if model.picker then
        div [ class "list" ]
            (store.exercises
                |> List.filter (not << .archived)
                |> List.map
                    (\ex ->
                        button
                            [ classList [ ( "row-btn", True ), ( "current", Maybe.map .exerciseId live.active == Just ex.id ) ]
                            , onClick (Act (SelectExercise ex.id))
                            ]
                            [ span [] [ text ex.name ]
                            , span [ class "muted" ] [ text (exerciseTimeLabel model live ex.id) ]
                            ]
                    )
            )

      else
        text ""
    ]


exerciseTimeLabel : Model -> LiveSession -> String -> String
exerciseTimeLabel model live exId =
    let
        t =
            Domain.exerciseTimeInLive model.now exId live
    in
    if t > 0 then
        clock t

    else
        ""


viewActive : Model -> LiveSession -> Active -> Html Msg
viewActive model live a =
    let
        ex =
            findExercise model.store a.exerciseId

        spent =
            Domain.exerciseTimeInLive model.now a.exerciseId live

        target =
            presetTarget model.store live

        running =
            a.runningSince /= Nothing

        countIn =
            Domain.countInRemaining model.now a

        lastTop =
            Domain.lastTopBpm a.exerciseId model.store.sessions

        over =
            target |> Maybe.map (\m -> spent >= m * 60000) |> Maybe.withDefault False

        bpmBtn delta =
            button [ class "btn bpm-btn", onClick (Act (BumpBpm delta)) ]
                [ text
                    (if delta > 0 then
                        "+" ++ String.fromInt delta

                     else
                        "−" ++ String.fromInt (abs delta)
                    )
                ]
    in
    div [ class "card active-card" ]
        [ div [ class "ex-name" ] [ text (ex |> Maybe.map .name |> Maybe.withDefault "(удалено)") ]
        , case ex |> Maybe.map .notes |> Maybe.withDefault "" of
            "" ->
                text ""

            notes ->
                div [ class "ex-notes" ] [ text notes ]
        , div [ classList [ ( "ex-time", True ), ( "over", over ) ] ]
            [ text (clock spent)
            , case target of
                Just m ->
                    span [ class "muted" ] [ text (" / " ++ String.fromInt m ++ ":00") ]

                Nothing ->
                    text ""
            ]
        , case target of
            Just m ->
                div [ class "progress" ]
                    [ div
                        [ class "progress-fill"
                        , style "width" (String.fromFloat (min 100 (toFloat spent / toFloat (m * 600))) ++ "%")
                        ]
                        []
                    ]

            Nothing ->
                text ""
        , div [ class "bpm-row" ]
            [ bpmBtn -5
            , bpmBtn -1
            , div [ class "bpm" ]
                [ div [ class "bpm-value" ] [ text (String.fromInt a.bpm) ]
                , div [ class "bpm-label" ] [ text "BPM" ]
                ]
            , bpmBtn 1
            , bpmBtn 5
            ]
        , div [ class "bpm-meta muted small" ]
            [ text
                ("старт "
                    ++ String.fromInt a.startBpm
                    ++ (if a.maxBpm > 0 then
                            " · макс сейчас " ++ String.fromInt a.maxBpm

                        else
                            ""
                       )
                    ++ (lastTop |> Maybe.map (\b -> " · прошлый макс " ++ String.fromInt b) |> Maybe.withDefault "")
                )
            ]
        , div [ class "seg" ]
            (List.map
                (\n ->
                    button
                        [ classList [ ( "seg-btn", True ), ( "on", a.subdivision == n ) ]
                        , onClick (Act (SetSubdivision n))
                        , title (subdivisionLabel n)
                        ]
                        [ text (subdivisionIcon n) ]
                )
                [ 1, 2, 3, 4 ]
                ++ [ button
                        [ classList [ ( "seg-btn", True ), ( "on", a.metronomeOn ) ]
                        , onClick (Act ToggleClick)
                        ]
                        [ text
                            (if a.metronomeOn then
                                "клик вкл"

                             else
                                "клик выкл"
                            )
                        ]
                   ]
            )
        , button
            [ classList [ ( "btn play", True ), ( "running", running ) ]
            , onClick (Act TogglePlay)
            ]
            [ text
                (if countIn > 0 then
                    "отсчёт… " ++ String.fromInt (ceiling (toFloat countIn * toFloat a.bpm / 60000))

                 else if running then
                    "❚❚ Пауза"

                 else
                    "▶ Играть"
                )
            ]
        ]


viewQueue : Model -> LiveSession -> Preset -> Html Msg
viewQueue model live preset =
    let
        current =
            live.active |> Maybe.map .exerciseId

        currentDone =
            case ( live.active, presetTarget model.store live ) of
                ( Just a, Just m ) ->
                    Domain.exerciseTimeInLive model.now a.exerciseId live >= m * 60000

                _ ->
                    False
    in
    div [ class "queue" ]
        [ div [ class "list" ]
            (List.map
                (\item ->
                    let
                        spent =
                            Domain.exerciseTimeInLive model.now item.exerciseId live

                        done =
                            spent >= item.minutes * 60000
                    in
                    button
                        [ classList [ ( "row-btn", True ), ( "current", current == Just item.exerciseId ), ( "done", done ) ]
                        , onClick (Act (SelectExercise item.exerciseId))
                        ]
                        [ span []
                            [ text
                                ((if done then
                                    "✓ "

                                  else
                                    ""
                                 )
                                    ++ exerciseName model.store item.exerciseId
                                )
                            ]
                        , span [ class "muted" ] [ text (clock spent ++ " / " ++ String.fromInt item.minutes ++ " мин") ]
                        ]
                )
                preset.items
            )
        , button
            [ classList [ ( "btn big next", True ), ( "glow", currentDone ) ]
            , onClick (Act NextExercise)
            ]
            [ text "Далее ▸" ]
        ]



-- VIEW: SUMMARY


viewSummary : Model -> Session -> List (Html Msg)
viewSummary model session =
    let
        tops =
            Domain.topBpmsInSession session

        exerciseIds =
            session.segments
                |> List.map .exerciseId
                |> List.foldl
                    (\id acc ->
                        if List.member id acc then
                            acc

                        else
                            acc ++ [ id ]
                    )
                    []

        timeOn id =
            session.segments |> List.filter (\s -> s.exerciseId == id) |> List.map .durationMs |> List.sum
    in
    [ h1 [] [ text "Сессия" ]
    , p [ class "muted" ] [ text (dateLabel model.zone session.startedAt) ]
    , div [ class "tiles" ] [ tile "Время" (clock (Domain.sessionTotal session)) ]
    , div [ class "list" ]
        (List.map
            (\id ->
                let
                    top =
                        Dict.get id tops |> Maybe.withDefault 0

                    ex =
                        findExercise model.store id
                in
                div [ class "card" ]
                    [ div [ class "row spread" ]
                        [ div [ class "card-title" ] [ text (exerciseName model.store id) ]
                        , div [] [ text (clock (timeOn id)) ]
                        ]
                    , if top > 0 && (Maybe.map .metronome ex |> Maybe.withDefault True) then
                        div [ class "muted" ] [ text ("макс " ++ String.fromInt top ++ " BPM") ]

                      else
                        text ""
                    , case ex of
                        Just e ->
                            if e.metronome && top > e.startBpm then
                                button [ class "btn small", onClick (PromoteBpm id top) ]
                                    [ text ("Сделать " ++ String.fromInt top ++ " стартовым BPM (сейчас " ++ String.fromInt e.startBpm ++ ")") ]

                            else if top == e.startBpm && top > 0 then
                                div [ class "muted small" ] [ text ("Стартовый BPM: " ++ String.fromInt e.startBpm) ]

                            else
                                text ""

                        Nothing ->
                            text ""
                    ]
            )
            exerciseIds
        )
    , a [ class "btn big", href (routeHref Home) ] [ text "Готово" ]
    , if model.confirmDelete then
        div [ class "row" ]
            [ button [ class "btn danger", onClick (DeleteSession session.id) ] [ text "Удалить навсегда" ]
            , button [ class "btn secondary", onClick (AskDelete False) ] [ text "Отмена" ]
            ]

      else
        button [ class "link danger", onClick (AskDelete True) ] [ text "Удалить эту сессию" ]
    ]



-- VIEW: LIBRARY


viewLibrary : Model -> List (Html Msg)
viewLibrary model =
    let
        ( active, archived ) =
            List.partition (not << .archived) model.store.exercises
    in
    [ h1 [] [ text "Библиотека" ]
    , h2 [] [ text "Пресеты" ]
    , div [ class "list" ]
        (List.map
            (\pr ->
                a [ class "row-btn", href (routeHref (EditPreset (Just pr.id))) ]
                    [ span [] [ text pr.name ]
                    , span [ class "muted" ] [ text (String.fromInt (List.sum (List.map .minutes pr.items)) ++ " мин") ]
                    ]
            )
            model.store.presets
        )
    , a [ class "btn secondary wide", href (routeHref (EditPreset Nothing)) ] [ text "+ Новый пресет" ]
    , h2 [] [ text "Упражнения" ]
    , div [ class "list" ]
        (List.map
            (\ex ->
                a [ class "row-btn", href (routeHref (EditExercise (Just ex.id))) ]
                    [ span [] [ text ex.name ]
                    , span [ class "muted" ]
                        [ text
                            (if ex.metronome then
                                String.fromInt ex.startBpm ++ " BPM " ++ subdivisionIcon ex.subdivision

                             else
                                "без клика"
                            )
                        ]
                    ]
            )
            active
        )
    , a [ class "btn secondary wide", href (routeHref (EditExercise Nothing)) ] [ text "+ Новое упражнение" ]
    , if List.isEmpty archived then
        text ""

      else
        details []
            [ summary [ class "muted" ] [ text ("Архив (" ++ String.fromInt (List.length archived) ++ ")") ]
            , div [ class "list" ]
                (List.map
                    (\ex ->
                        div [ class "row-btn" ]
                            [ span [] [ text ex.name ]
                            , button [ class "link", onClick (RestoreExercise ex.id) ] [ text "Вернуть" ]
                            ]
                    )
                    archived
                )
            ]
    ]


viewNotice : Model -> Html msg
viewNotice model =
    case model.notice of
        Just ( ok, message ) ->
            div [ classList [ ( "notice", True ), ( "error", not ok ) ] ] [ text message ]

        Nothing ->
            text ""


viewExerciseForm : Model -> ExerciseForm -> List (Html Msg)
viewExerciseForm model f =
    [ h1 []
        [ text
            (if f.id == Nothing then
                "Новое упражнение"

             else
                "Упражнение"
            )
        ]
    , label [ class "field" ]
        [ span [] [ text "Название" ]
        , input [ value f.name, onInput ExName, placeholder "Свип-арпеджио, ля минор" ] []
        ]
    , label [ class "field" ]
        [ span [] [ text "Стартовый BPM" ]
        , input [ type_ "number", attribute "inputmode" "numeric", value f.bpm, onInput ExBpm ] []
        ]
    , div [ class "field" ]
        [ span [] [ text "Кликов на долю" ]
        , div [ class "seg" ]
            (List.map
                (\n ->
                    button
                        [ classList [ ( "seg-btn", True ), ( "on", f.subdivision == n ) ]
                        , onClick (ExSubdivision n)
                        ]
                        [ text (subdivisionIcon n ++ " " ++ subdivisionLabel n) ]
                )
                [ 1, 2, 3, 4 ]
            )
        ]
    , label [ class "field check" ]
        [ input [ type_ "checkbox", checked f.metronome, onCheck ExMetronome ] []
        , span [] [ text "Метроном включён по умолчанию" ]
        ]
    , label [ class "field" ]
        [ span [] [ text "Заметки" ]
        , textarea [ value f.notes, onInput ExNotes, rows 3, placeholder "Ссылка на табы, на чём сосредоточиться…" ] []
        ]
    , viewNotice model
    , button [ class "btn big", onClick SaveExercise ] [ text "Сохранить" ]
    , case f.id of
        Just id ->
            button [ class "link danger", onClick (DeleteExercise id) ] [ text "Удалить упражнение" ]

        Nothing ->
            text ""
    ]


viewPresetForm : Model -> PresetForm -> List (Html Msg)
viewPresetForm model f =
    let
        exercises =
            List.filter (not << .archived) model.store.exercises

        exerciseOptions selected =
            List.map
                (\ex -> option [ value ex.id, Html.Attributes.selected (ex.id == selected) ] [ text ex.name ])
                exercises

        total =
            f.items |> List.filterMap (.minutes >> String.toInt) |> List.sum
    in
    [ h1 []
        [ text
            (if f.id == Nothing then
                "Новый пресет"

             else
                "Пресет"
            )
        ]
    , label [ class "field" ]
        [ span [] [ text "Название" ]
        , input [ value f.name, onInput PrName, placeholder "Ежедневный" ] []
        ]
    , div [ class "field" ]
        [ span [] [ text ("Упражнения · " ++ String.fromInt total ++ " мин") ]
        , div [ class "list" ] (List.indexedMap (viewPresetItem model.drag (List.length f.items) exerciseOptions) f.items)
        , button [ class "btn secondary wide", onClick TogglePicker ] [ text "+ Добавить упражнение" ]
        , if model.picker then
            div [ class "list" ]
                (List.map
                    (\ex -> button [ class "row-btn", onClick (PrAddItem ex.id) ] [ span [] [ text ex.name ] ])
                    exercises
                )

          else
            text ""
        ]
    , viewNotice model
    , button [ class "btn big", onClick SavePreset ] [ text "Сохранить" ]
    , case f.id of
        Just id ->
            button [ class "link danger", onClick (DeletePreset id) ] [ text "Удалить пресет" ]

        Nothing ->
            text ""
    ]


viewPresetItem : Maybe Drag -> Int -> (String -> List (Html Msg)) -> Int -> { exerciseId : String, minutes : String } -> Html Msg
viewPresetItem drag count exerciseOptions i item =
    let
        px v =
            "translateY(" ++ String.fromFloat v ++ "px)"

        ( dragging, shift ) =
            case drag of
                Just d ->
                    let
                        target =
                            dropIndex count d
                    in
                    if i == d.from then
                        ( True, px (d.y - d.startY) )

                    else if d.from < i && i <= target then
                        ( False, px -d.pitch )

                    else if target <= i && i < d.from then
                        ( False, px d.pitch )

                    else
                        ( False, "none" )

                Nothing ->
                    ( False, "none" )

        startDrag =
            D.map2 (\y height -> ( DragStart i y (height + 8), True ))
                (D.field "clientY" D.float)
                (D.at [ "currentTarget", "parentElement", "offsetHeight" ] D.float)
    in
    div [ classList [ ( "preset-item", True ), ( "dragging", dragging ), ( "settling", drag /= Nothing && not dragging ) ], style "transform" shift ]
        [ span
            [ class "drag-handle"
            , preventDefaultOn "pointerdown" startDrag

            -- Touch pointers are implicitly captured by the handle, so its own
            -- move/up events keep coming while the finger leaves it.
            , on "pointermove" (D.map DragMove (D.field "clientY" D.float))
            , on "pointerup" (D.succeed DragEnd)
            , on "pointercancel" (D.succeed DragEnd)
            ]
            [ text "⠿" ]
        , select [ onInput (PrItemExercise i) ] (exerciseOptions item.exerciseId)
        , input
            [ type_ "number"
            , attribute "inputmode" "numeric"
            , class "minutes"
            , value item.minutes
            , onInput (PrItemMinutes i)
            ]
            []
        , span [ class "muted" ] [ text "мин" ]
        , button [ class "icon-btn danger", onClick (PrItemRemove i) ] [ text "✕" ]
        ]



-- VIEW: STATS


{-| (day label, practice millis) for the last `n` days, oldest first.
-}
lastDays : Model -> Int -> List ( String, Int )
lastDays model n =
    let
        byDay =
            Domain.minutesByDay model.zone model.store.sessions

        dayMs =
            24 * 60 * 60 * 1000
    in
    List.range 0 (n - 1)
        |> List.reverse
        |> List.map
            (\offset ->
                let
                    t =
                        model.now - offset * dayMs
                in
                ( String.fromInt (Time.toDay model.zone (Time.millisToPosix t))
                , Dict.get (Domain.dayKey model.zone t) byDay |> Maybe.withDefault 0
                )
            )


viewStats : Model -> List (Html Msg)
viewStats model =
    let
        sessions =
            model.store.sessions

        days =
            lastDays model 14

        maxDay =
            days |> List.map Tuple.second |> List.maximum |> Maybe.withDefault 0 |> max 1

        totals =
            Domain.exerciseTotals sessions

        all =
            Dict.values totals |> List.sum

        ranked =
            Dict.toList totals |> List.sortBy (Tuple.second >> negate)
    in
    [ h1 [] [ text "Статистика" ]
    , div [ class "tiles" ]
        [ tile "7 дней" (duration (lastDays model 7 |> List.map Tuple.second |> List.sum))
        , tile "Всего" (duration all)
        , tile "Сессий" (String.fromInt (List.length sessions))
        ]
    , h2 [] [ text "Последние 14 дней" ]
    , div [ class "bars" ]
        (List.map
            (\( label, ms ) ->
                div [ class "bar-col" ]
                    [ div [ class "bar-val" ]
                        [ text
                            (if ms > 0 then
                                String.fromInt (ms // 60000)

                             else
                                ""
                            )
                        ]
                    , div [ class "bar-track" ]
                        [ div [ class "bar", style "height" (String.fromFloat (100 * toFloat ms / toFloat maxDay) ++ "%") ] [] ]
                    , div [ class "bar-label" ] [ text label ]
                    ]
            )
            days
        )
    , h2 [] [ text "Упражнения" ]
    , if List.isEmpty ranked then
        p [ class "muted" ] [ text "Пока пусто. Иди играть!" ]

      else
        div [ class "list" ]
            (List.map
                (\( id, ms ) ->
                    let
                        history =
                            Domain.topBpmHistory id sessions
                    in
                    div [ class "card ex-stat" ]
                        [ div [ class "row spread" ]
                            [ div [ class "card-title" ] [ text (exerciseName model.store id) ]
                            , div [] [ text (duration ms) ]
                            ]
                        , if findExercise model.store id |> Maybe.map .metronome |> Maybe.withDefault True then
                            viewBpmHistory history

                          else
                            text ""
                        ]
                )
                ranked
            )
    , h2 [] [ text "История" ]
    , div [ class "list" ]
        (sessions
            |> List.take 30
            |> List.map
                (\s ->
                    a [ class "row-btn", href (routeHref (Summary s.id)) ]
                        [ span [] [ text (dateLabel model.zone s.startedAt) ]
                        , span [ class "muted" ]
                            [ text
                                ((s.presetId |> Maybe.andThen (findPreset model.store) |> Maybe.map (\pr -> pr.name ++ " · ") |> Maybe.withDefault "")
                                    ++ duration (Domain.sessionTotal s)
                                )
                            ]
                        ]
                )
        )
    ]


{-| Top BPM per session as a sparkline, with first → latest values.
-}
viewBpmHistory : List ( Int, Int ) -> Html msg
viewBpmHistory history =
    let
        bpms =
            List.map Tuple.second history

        lo =
            List.minimum bpms |> Maybe.withDefault 0

        hi =
            List.maximum bpms |> Maybe.withDefault 0

        w =
            300

        h =
            48

        n =
            List.length bpms

        x i =
            if n <= 1 then
                toFloat w / 2

            else
                toFloat i * toFloat w / toFloat (n - 1)

        y b =
            if hi == lo then
                toFloat h / 2

            else
                4 + (toFloat (hi - b) / toFloat (hi - lo)) * toFloat (h - 8)

        points =
            List.indexedMap (\i b -> String.fromFloat (x i) ++ "," ++ String.fromFloat (y b)) bpms
                |> String.join " "
    in
    case ( List.head bpms, List.reverse bpms |> List.head ) of
        ( Just first, Just latest ) ->
            div [ class "spark" ]
                [ if n < 2 then
                    text ""

                  else
                    Svg.svg [ SA.viewBox ("0 0 " ++ String.fromInt w ++ " " ++ String.fromInt h), SA.preserveAspectRatio "none" ]
                        [ Svg.polyline [ SA.points points, SA.fill "none", SA.stroke "currentColor", SA.strokeWidth "2", attribute "vector-effect" "non-scaling-stroke" ] []
                        ]
                , div [ class "muted small" ]
                    [ text
                        ("макс BPM "
                            ++ String.fromInt first
                            ++ " → "
                            ++ String.fromInt latest
                            ++ " · лучший "
                            ++ String.fromInt hi
                            ++ " · "
                            ++ String.fromInt n
                            ++ " "
                            ++ plural n "сессия" "сессии" "сессий"
                        )
                    ]
                ]

        _ ->
            text ""



-- VIEW: SETTINGS


viewSettings : Model -> List (Html Msg)
viewSettings model =
    [ h1 [] [ text "Настройки" ]
    , label [ class "field" ]
        [ span [] [ text ("Громкость клика · " ++ String.fromInt (round (model.store.settings.volume * 100)) ++ "%") ]
        , input
            [ type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max "1"
            , Html.Attributes.step "0.05"
            , value (String.fromFloat model.store.settings.volume)
            , onInput (String.toFloat >> Maybe.withDefault 0.8 >> SetVolume)
            ]
            []
        ]
    , h2 [] [ text "Резервная копия" ]
    , p [ class "muted small" ] [ text "Все данные хранятся только на этом телефоне. Время от времени делай экспорт, чтобы их не потерять." ]
    , div [ class "row" ]
        [ button [ class "btn secondary", onClick Export ] [ text "Экспорт" ]
        , button [ class "btn secondary", onClick ImportFile ] [ text "Импорт из файла" ]
        ]
    , h2 [] [ text "Вставить JSON" ]
    , p [ class "muted small" ]
        [ text "Добавляет или обновляет упражнения и пресеты (по названию). История не меняется. Полная резервная копия заменяет всё." ]
    , textarea [ class "paste", value model.importText, onInput ImportTextChanged, rows 6, placeholder importExample ] []
    , viewNotice model
    , button [ class "btn", onClick ApplyImport, disabled (String.trim model.importText == "") ] [ text "Импортировать" ]
    , details []
        [ summary [ class "muted" ] [ text "Формат" ]
        , pre [ class "code" ] [ text importExample ]
        ]
    ]


importExample : String
importExample =
    """{
  "exercises": [
    { "name": "Свипы Am", "bpm": 90, "subdivision": 3,
      "metronome": true, "notes": "5 струн" }
  ],
  "presets": [
    { "name": "Ежедневный", "items": [
      { "exercise": "Свипы Am", "minutes": 10 }
    ] }
  ]
}"""



-- MAIN


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.store.live /= Nothing then
            Time.every 250 (\_ -> Act Tick)

          else
            Time.every 30000 (\_ -> Act Tick)

        -- A mouse isn't captured by the drag handle; follow it document-wide.
        , if model.drag /= Nothing then
            Sub.batch
                [ Browser.Events.onMouseMove (D.map DragMove (D.field "clientY" D.float))
                , Browser.Events.onMouseUp (D.succeed DragEnd)
                ]

          else
            Sub.none
        ]


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
