port module Main exposing (main)

import Bpm
import Browser
import Browser.Events
import Browser.Navigation as Nav
import Dict
import Exercise exposing (Exercise, Subdivision)
import File exposing (File)
import File.Download
import File.Select
import Format
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Keyed
import Json.Decode as D
import Json.Encode as E
import Random
import Session
import Stats
import Store exposing (Store)
import Svg
import Svg.Attributes as SA
import Task
import Theme exposing (Theme)
import Time
import Url exposing (Url)



-- PORTS


port saveState : E.Value -> Cmd msg


{-| `{ running: false }` or `{ running: true, bpm, subdivision, volume }`.
-}
port metronome : E.Value -> Cmd msg


port wakeLock : Bool -> Cmd msg


port theme : String -> Cmd msg


{-| Whether the phone is in dark mode, whenever that changes.
-}
port systemDark : (Bool -> msg) -> Sub msg



-- MODEL


type alias Model =
    { key : Nav.Key
    , page : Page
    , pushed : Bool
    , store : Store
    , persistence : Persistence
    , now : Int
    , zone : Time.Zone
    , videoOpen : Bool
    , startupNotice : Maybe String
    , systemDark : Bool
    }


{-| `ReadOnly` when the saved data couldn't be read: it is left untouched
until the user restores a backup.
-}
type Persistence
    = Persist
    | ReadOnly


{-| The panel over the metronome screen, with its own state. The metronome
screen itself stays rendered underneath, so a playing video isn't torn down.
-}
type Page
    = Home
    | Pick PickState
    | Edit EditState
    | EditMissing
    | Stats
    | Settings SettingsState


type alias PickState =
    { query : String
    , suggested : Maybe String
    }


type alias EditState =
    { form : Form
    , confirmDelete : Bool
    , error : Maybe String
    }


type alias Form =
    { id : String
    , name : String
    , bpm : String
    , subdivision : Subdivision
    , video : String
    }


type alias SettingsState =
    { importText : String
    , message : Maybe String
    , themeSlot : ThemeSlot
    }


{-| Which theme the settings are editing: the one for the phone's light
mode or for its dark mode.
-}
type ThemeSlot
    = Day
    | Night


type alias Flags =
    { stored : Maybe String
    , legacy : Maybe String
    , now : Int
    , systemDark : Bool
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( loaded, persistence, notice ) =
            case ( flags.stored, flags.legacy ) of
                ( Just json, _ ) ->
                    case D.decodeString Store.decoder json of
                        Ok s ->
                            ( s, Persist, Nothing )

                        Err _ ->
                            ( Store.empty, ReadOnly, Nothing )

                ( Nothing, Just json ) ->
                    case D.decodeString Store.legacyDecoder json of
                        Ok s ->
                            ( s, Persist, Nothing )

                        Err _ ->
                            ( Store.empty, Persist, Just "Не удалось перенести историю из старой версии." )

                ( Nothing, Nothing ) ->
                    ( Store.empty, Persist, Nothing )

        store =
            Store.closeIfIdle flags.now loaded

        model =
            { key = key
            , page = pageFor store flags.systemDark (parseRoute url)
            , pushed = False
            , store = store
            , persistence = persistence
            , now = flags.now
            , zone = Time.utc
            , videoOpen = False
            , startupNotice = notice
            , systemDark = flags.systemDark
            }
    in
    ( model
    , Cmd.batch
        [ Task.perform GotZone Time.here
        , theme (Theme.id (shownTheme model))
        , if persistence == Persist then
            saveState (Store.encode store)

          else
            Cmd.none
        ]
    )



-- ROUTING


type Route
    = HomeRoute
    | PickRoute
    | EditRoute String
    | StatsRoute
    | SettingsRoute


parseRoute : Url -> Route
parseRoute url =
    case String.split "/" (Maybe.withDefault "" url.fragment) |> List.filter ((/=) "") of
        [ "pick" ] ->
            PickRoute

        [ "ex", id ] ->
            EditRoute id

        [ "stats" ] ->
            StatsRoute

        [ "settings" ] ->
            SettingsRoute

        _ ->
            HomeRoute


routeHref : Route -> String
routeHref route =
    "#/"
        ++ (case route of
                HomeRoute ->
                    ""

                PickRoute ->
                    "pick"

                EditRoute id ->
                    "ex/" ++ id

                StatsRoute ->
                    "stats"

                SettingsRoute ->
                    "settings"
           )


pageFor : Store -> Bool -> Route -> Page
pageFor store dark route =
    case route of
        HomeRoute ->
            Home

        PickRoute ->
            Pick { query = "", suggested = Nothing }

        EditRoute id ->
            case Store.findExercise store id of
                Just e ->
                    Edit
                        { form =
                            { id = e.id
                            , name = e.name
                            , bpm = String.fromInt (Bpm.toInt e.startBpm)
                            , subdivision = e.subdivision
                            , video = Maybe.withDefault "" e.video
                            }
                        , confirmDelete = False
                        , error = Nothing
                        }

                Nothing ->
                    EditMissing

        StatsRoute ->
            Stats

        SettingsRoute ->
            Settings { importText = "", message = Nothing, themeSlot = slotFor dark }


{-| Back to the previous screen: `back` when we navigated here (so Android's
back gesture and ✕ agree), otherwise replace with Home.
-}
close : Model -> Cmd Msg
close model =
    if model.pushed then
        Nav.back model.key 1

    else
        Nav.replaceUrl model.key (routeHref HomeRoute)



-- UPDATE


{-| Actions that need the exact current time.
-}
type Action
    = TogglePlay
    | Bump Int
    | Choose (Maybe String)
    | SubmitSearch String
    | Create String
    | Suggest
    | DeleteExercise String
    | ApplyImport String
    | Tick


type Msg
    = Act Action
    | Run Action Time.Posix
    | GotZone Time.Zone
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | VisibilityChanged Browser.Events.Visibility
    | Close
    | ToggleClick
    | CycleSubdivision
    | MakeStart
    | ToggleVideo
    | StartBump String Int
    | QueryChanged String
    | FormName String
    | FormBpm String
    | FormBpmBump Int
    | FormSubdivision Subdivision
    | FormVideo String
    | SaveForm
    | AskDelete
    | SetTheme ThemeSlot Theme
    | PickThemeSlot ThemeSlot
    | SystemDarkChanged Bool
    | SetVolume Float
    | Export
    | ImportFile
    | FileSelected File
    | FileLoaded String
    | ImportTextChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( next, cmd ) =
            updateModel msg model
    in
    ( next, Cmd.batch [ cmd, effects model next ] )


{-| Side effects that follow from state changes.
-}
effects : Model -> Model -> Cmd Msg
effects old new =
    Cmd.batch
        [ if click old /= click new then
            metronome (encodeClick (click new))

          else
            Cmd.none
        , if Store.isRunning old.store /= Store.isRunning new.store then
            wakeLock (Store.isRunning new.store)

          else
            Cmd.none
        , if old.store /= new.store && new.persistence == Persist then
            saveState (Store.encode new.store)

          else
            Cmd.none
        , if shownTheme old /= shownTheme new then
            theme (Theme.id (shownTheme new))

          else
            Cmd.none
        ]


type alias Click =
    { bpm : Int, subdivision : Int, volume : Float }


click : Model -> Maybe Click
click { store } =
    if Store.isRunning store && Session.clickOn store.deck then
        Just
            { bpm = Bpm.toInt (Session.bpm store.deck)
            , subdivision = Exercise.perBeat (Store.subdivision store)
            , volume = store.settings.volume
            }

    else
        Nothing


encodeClick : Maybe Click -> E.Value
encodeClick c =
    case c of
        Just { bpm, subdivision, volume } ->
            E.object
                [ ( "running", E.bool True )
                , ( "bpm", E.int bpm )
                , ( "subdivision", E.int subdivision )
                , ( "volume", E.float volume )
                ]

        Nothing ->
            E.object [ ( "running", E.bool False ) ]


updateModel : Msg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
    case msg of
        Act action ->
            ( model, Task.perform (Run action) Time.now )

        Run action posix ->
            let
                now =
                    Time.posixToMillis posix
            in
            act action now { model | now = now }

        GotZone zone ->
            ( { model | zone = zone }, Cmd.none )

        LinkClicked (Browser.Internal url) ->
            ( { model | pushed = True }, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            let
                route =
                    parseRoute url
            in
            ( { model
                | page = pageFor model.store model.systemDark route
                , pushed = model.pushed && route /= HomeRoute
                , startupNotice = Nothing
              }
              -- "Today" may have moved on while nothing was ticking.
            , Task.perform (Run Tick) Time.now
            )

        VisibilityChanged Browser.Events.Visible ->
            ( model, Task.perform (Run Tick) Time.now )

        VisibilityChanged Browser.Events.Hidden ->
            ( model, Cmd.none )

        Close ->
            ( model, close model )

        ToggleClick ->
            ( mapStore (\s -> { s | deck = Session.toggleClick s.deck }) model, Cmd.none )

        CycleSubdivision ->
            ( mapStore (\s -> Store.setSubdivision (Exercise.nextSubdivision (Store.subdivision s)) s) model, Cmd.none )

        MakeStart ->
            ( mapStore
                (\s ->
                    case Store.currentExercise s of
                        Just e ->
                            Store.updateExercise e.id (\x -> { x | startBpm = Session.bpm s.deck }) s

                        Nothing ->
                            s
                )
                model
            , Cmd.none
            )

        ToggleVideo ->
            ( { model | videoOpen = not model.videoOpen }, Cmd.none )

        StartBump id delta ->
            ( mapStore (Store.updateExercise id (\e -> { e | startBpm = Bpm.add delta e.startBpm })) model, Cmd.none )

        QueryChanged q ->
            ( mapPick (\p -> { p | query = q }) model, Cmd.none )

        FormName s ->
            ( mapForm (\f -> { f | name = s }) model, Cmd.none )

        FormBpm s ->
            ( mapForm (\f -> { f | bpm = s }) model, Cmd.none )

        FormBpmBump delta ->
            ( mapForm
                (\f ->
                    { f
                        | bpm =
                            String.toInt f.bpm
                                |> Maybe.withDefault 80
                                |> Bpm.fromInt
                                |> Bpm.add delta
                                |> Bpm.toInt
                                |> String.fromInt
                    }
                )
                model
            , Cmd.none
            )

        FormSubdivision sub ->
            ( mapForm (\f -> { f | subdivision = sub }) model, Cmd.none )

        FormVideo s ->
            ( mapForm (\f -> { f | video = s }) model, Cmd.none )

        SaveForm ->
            case model.page of
                Edit state ->
                    case validate state.form of
                        Ok apply ->
                            ( mapStore (Store.updateExercise state.form.id apply) model, close model )

                        Err error ->
                            ( { model | page = Edit { state | error = Just error } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AskDelete ->
            case model.page of
                Edit state ->
                    ( { model | page = Edit { state | confirmDelete = True } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetTheme slot t ->
            ( mapSettings (setSlotTheme slot t) model, Cmd.none )

        PickThemeSlot slot ->
            ( mapSettingsPage (\p -> { p | themeSlot = slot }) model, Cmd.none )

        SystemDarkChanged dark ->
            ( { model | systemDark = dark }, Cmd.none )

        SetVolume v ->
            ( mapSettings (\s -> { s | volume = clamp 0 1 v }) model, Cmd.none )

        Export ->
            ( model
            , File.Download.string
                ("guitar-" ++ Format.dayStamp model.zone model.now ++ ".json")
                "application/json"
                (E.encode 2 (Store.encode model.store))
            )

        ImportFile ->
            ( model, File.Select.file [ "application/json", ".json" ] FileSelected )

        FileSelected file ->
            ( model, Task.perform FileLoaded (File.toString file) )

        FileLoaded text ->
            ( model, Task.perform (Run (ApplyImport text)) Time.now )

        ImportTextChanged s ->
            ( mapSettingsPage (\p -> { p | importText = s }) model, Cmd.none )


validate : Form -> Result String (Exercise -> Exercise)
validate f =
    case ( String.trim f.name, String.toInt (String.trim f.bpm) ) of
        ( "", _ ) ->
            Err "Нужно название."

        ( _, Nothing ) ->
            Err "Темп должен быть числом."

        ( name, Just bpm ) ->
            Ok
                (\e ->
                    { e
                        | name = name
                        , startBpm = Bpm.fromInt bpm
                        , subdivision = f.subdivision
                        , video =
                            if String.trim f.video == "" then
                                Nothing

                            else
                                Just (String.trim f.video)
                    }
                )


act : Action -> Int -> Model -> ( Model, Cmd Msg )
act action now model0 =
    let
        model =
            mapStore (Store.closeIfIdle now) model0
    in
    case action of
        Tick ->
            ( mapStore (Store.tick now) model, Cmd.none )

        TogglePlay ->
            ( mapStore (Store.togglePlay now) model, Cmd.none )

        Bump delta ->
            ( mapStore (Store.bump now delta) model, Cmd.none )

        Choose exerciseId ->
            ( choose now exerciseId model, close model )

        SubmitSearch query ->
            -- Done on the keyboard: open an exact match, add only when
            -- nothing matches, otherwise leave the filtered list as is.
            let
                found =
                    search query model.store.exercises
            in
            case ( found.exact, found.matches ) of
                ( Just e, _ ) ->
                    act (Choose (Just e.id)) now model

                ( Nothing, [] ) ->
                    act (Create query) now model

                _ ->
                    ( model, Cmd.none )

        Create query ->
            let
                found =
                    search query model.store.exercises

                id =
                    "e" ++ String.fromInt now
            in
            if found.query == "" || found.exact /= Nothing then
                ( model, Cmd.none )

            else
                ( model
                    |> mapStore
                        (\s ->
                            Store.addExercise
                                { id = id
                                , name = String.trim query
                                , startBpm = Session.bpm s.deck
                                , subdivision = Store.subdivision s
                                , video = Nothing
                                }
                                s
                        )
                    |> choose now (Just id)
                , close model
                )

        Suggest ->
            case model.page of
                Pick state ->
                    let
                        played =
                            Stats.lastPlayed (Store.allSegments now model.store)

                        candidates =
                            model.store.exercises
                                |> List.filter (\e -> Just e.id /= Session.exerciseId model.store.deck && Just e.id /= state.suggested)
                                |> List.sortBy (\e -> Dict.get e.id played |> Maybe.withDefault 0)
                                |> List.take 3

                        ( pick, _ ) =
                            Random.step (Random.int 0 (List.length candidates - 1)) (Random.initialSeed now)
                    in
                    ( { model | page = Pick { state | suggested = candidates |> List.drop pick |> List.head |> Maybe.map .id } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DeleteExercise id ->
            ( mapStore (Store.deleteExercise now id) model, close model )

        ApplyImport text ->
            case Store.decodeImport text of
                Ok imp ->
                    let
                        imported =
                            model
                                |> mapStore (Store.applyImport now imp)
                                |> mapSettingsPage (\p -> { p | importText = "", message = Just (importMessage imp) })
                    in
                    case imp of
                        Store.FullBackup _ ->
                            -- A readable backup also lifts read-only mode.
                            ( { imported | persistence = Persist, videoOpen = False }, Cmd.none )

                        _ ->
                            ( imported, Cmd.none )

                Err _ ->
                    ( mapSettingsPage (\p -> { p | message = Just "Не получилось разобрать JSON." }) model, Cmd.none )


importMessage : Store.Import -> String
importMessage imp =
    case imp of
        Store.FullBackup _ ->
            "Резервная копия восстановлена."

        Store.HistoryOnly _ ->
            "История из старой версии добавлена."

        Store.Exercises list ->
            "Упражнений в файле: " ++ String.fromInt (List.length list) ++ "."


choose : Int -> Maybe String -> Model -> Model
choose now exerciseId model =
    let
        switched =
            mapStore (Store.switchTo now exerciseId) model
    in
    if Session.exerciseId switched.store.deck /= Session.exerciseId model.store.deck then
        { switched | videoOpen = False }

    else
        switched


type alias Search =
    { query : String
    , exact : Maybe Exercise
    , matches : List Exercise
    }


{-| Case-insensitive search by name, shared by the list and the keyboard's
Done so they can't disagree.
-}
search : String -> List Exercise -> Search
search query exercises =
    let
        q =
            String.toLower (String.trim query)
    in
    { query = q
    , exact = List.filter (\e -> String.toLower e.name == q) exercises |> List.head
    , matches = List.filter (\e -> String.contains q (String.toLower e.name)) exercises
    }



-- HELPERS


slotFor : Bool -> ThemeSlot
slotFor dark =
    if dark then
        Night

    else
        Day


slotTheme : ThemeSlot -> Store.Settings -> Theme
slotTheme slot settings =
    case slot of
        Day ->
            settings.dayTheme

        Night ->
            settings.nightTheme


setSlotTheme : ThemeSlot -> Theme -> Store.Settings -> Store.Settings
setSlotTheme slot t settings =
    case slot of
        Day ->
            { settings | dayTheme = t }

        Night ->
            { settings | nightTheme = t }


{-| The theme on screen: the one for the phone's current mode, or, while the
settings are open, the one being edited (as a preview).
-}
shownTheme : Model -> Theme
shownTheme model =
    case model.page of
        Settings { themeSlot } ->
            slotTheme themeSlot model.store.settings

        _ ->
            slotTheme (slotFor model.systemDark) model.store.settings


mapStore : (Store -> Store) -> Model -> Model
mapStore f model =
    { model | store = f model.store }


mapSettings : (Store.Settings -> Store.Settings) -> Model -> Model
mapSettings f =
    mapStore (\s -> { s | settings = f s.settings })


mapPick : (PickState -> PickState) -> Model -> Model
mapPick f model =
    case model.page of
        Pick state ->
            { model | page = Pick (f state) }

        _ ->
            model


mapForm : (Form -> Form) -> Model -> Model
mapForm f model =
    case model.page of
        Edit state ->
            { model | page = Edit { state | form = f state.form, error = Nothing } }

        _ ->
            model


mapSettingsPage : (SettingsState -> SettingsState) -> Model -> Model
mapSettingsPage f model =
    case model.page of
        Settings state ->
            { model | page = Settings (f state) }

        _ ->
            model



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Гитара"
    , body =
        [ div [ class "app" ]
            [ viewHome model
            , case model.page of
                Home ->
                    text ""

                Pick state ->
                    sheet "Что играешь?" (viewPicker model state)

                Edit state ->
                    sheet "Упражнение" (viewEdit state)

                EditMissing ->
                    sheet "Упражнение" [ p [ class "muted" ] [ text "Упражнение не найдено." ] ]

                Stats ->
                    sheet "Статистика" (viewStats model)

                Settings state ->
                    sheet "Настройки" (viewSettings model state)
            ]
        ]
    }


sheet : String -> List (Html Msg) -> Html Msg
sheet title content =
    div [ class "sheet" ]
        [ div [ class "sheet-head" ]
            [ h1 [] [ text title ]
            , button [ class "icon-btn", onClick Close, attribute "aria-label" "Закрыть" ] [ icon Cross ]
            ]
        , div [ class "sheet-body" ] content
        ]


viewHome : Model -> Html Msg
viewHome model =
    let
        store =
            model.store

        deck =
            store.deck

        bpm =
            Bpm.toInt (Session.bpm deck)

        ex =
            Store.currentExercise store

        video =
            ex |> Maybe.andThen .video

        videoId =
            video |> Maybe.andThen Exercise.youtubeId

        showVideo =
            model.videoOpen && videoId /= Nothing

        countIn =
            store.live |> Maybe.map (Session.countInRemaining model.now) |> Maybe.withDefault 0

        running =
            Store.isRunning store

        ( ringState, hint ) =
            if not running then
                ( "off", "тап — старт" )

            else if countIn > 0 then
                ( "on", "отсчёт · " ++ String.fromInt ((countIn * bpm + 59999) // 60000) )

            else if Session.clickOn deck then
                ( "on", "тап — пауза" )

            else
                ( "on silent", "тап — пауза" )

        timer f =
            store.live |> Maybe.map (f model.now deck) |> Maybe.withDefault 0

        subdivision =
            Store.subdivision store
    in
    main_
        [ classList [ ( "home", True ), ( "with-video", showVideo ) ] ]
        [ div [ class "topbar" ]
            [ a [ class "icon-btn", href (routeHref StatsRoute), attribute "aria-label" "Статистика" ] [ icon Chart ]
            , a [ class "icon-btn", href (routeHref SettingsRoute), attribute "aria-label" "Настройки" ] [ icon Gear ]
            ]
        , viewBanner model
        , div [ class "pill" ]
            [ a [ class "pill-main", href (routeHref PickRoute) ]
                [ span [ class "pill-name" ] [ text (ex |> Maybe.map .name |> Maybe.withDefault "Без упражнения") ]
                , small [] [ text "сменить ⌄" ]
                ]
            , case ( video, videoId ) of
                ( _, Just _ ) ->
                    button [ class "pill-side", onClick ToggleVideo ]
                        [ text
                            (if showVideo then
                                "скрыть"

                             else
                                "▶ видео"
                            )
                        ]

                ( Just link, Nothing ) ->
                    a [ class "pill-side", href link, target "_blank", rel "noopener" ] [ text "видео ↗" ]

                ( Nothing, Nothing ) ->
                    text ""
            ]
        , Html.Keyed.node "div"
            [ class "video-slot" ]
            [ case ( showVideo, videoId, video ) of
                ( True, Just vid, Just link ) ->
                    ( vid
                    , div [ class "video" ]
                        [ iframe
                            [ src ("https://www.youtube-nocookie.com/embed/" ++ vid ++ "?playsinline=1&rel=0")
                            , attribute "allow" "autoplay; encrypted-media; picture-in-picture; fullscreen"
                            , attribute "frameborder" "0"
                            , title "Видео"
                            ]
                            []
                        , a [ class "video-out", href link, target "_blank", rel "noopener" ] [ text "открыть в YouTube ↗" ]
                        ]
                    )

                _ ->
                    ( "none", text "" )
            ]
        , button [ class ("ring " ++ ringState), onClick (Act TogglePlay), attribute "aria-label" hint ]
            -- `.arc` and `.subs i` are animated from main.ts; Elm never styles them.
            [ span [ class "arc" ] []
            , span [ class "bpm" ] [ text (String.fromInt bpm) ]
            , span [ class "hint" ] [ text hint ]
            , if Exercise.perBeat subdivision > 1 then
                span [ class "subs" ] (List.repeat (Exercise.perBeat subdivision) (i [] []))

              else
                text ""
            ]
        , div [ class "chips" ]
            (case ex of
                Just e ->
                    [ if e.startBpm /= Session.bpm deck then
                        button [ class "chip", onClick MakeStart ]
                            [ text ("старт " ++ String.fromInt (Bpm.toInt e.startBpm) ++ " · ")
                            , b [] [ text ("сделать " ++ String.fromInt bpm) ]
                            ]

                      else
                        span [ class "chip quiet" ] [ text ("старт " ++ String.fromInt bpm) ]
                    ]

                Nothing ->
                    []
            )
        , div [ class "keys" ]
            (List.map
                (\( delta, label ) -> button [ class "key", onClick (Act (Bump delta)) ] [ text label ])
                [ ( -5, "−5" ), ( -1, "−1" ), ( 1, "+1" ), ( 5, "+5" ) ]
            )
        , div [ class "chips" ]
            [ button [ class "chip", onClick CycleSubdivision ] [ text (Format.subdivision subdivision) ]
            , button [ classList [ ( "chip", True ), ( "dim", not (Session.clickOn deck) ) ], onClick ToggleClick ]
                [ text
                    (if Session.clickOn deck then
                        "клик вкл"

                     else
                        "клик выкл"
                    )
                ]
            ]
        , div [ class "timers" ]
            [ div [] [ b [] [ text (Format.clock (timer Session.exerciseTime)) ], text "на упражнении" ]
            , div [] [ b [] [ text (Format.clock (timer Session.total)) ], text "всего" ]
            ]
        ]


viewBanner : Model -> Html msg
viewBanner model =
    case ( model.persistence, model.startupNotice ) of
        ( ReadOnly, _ ) ->
            p [ class "notice" ] [ text "Не удалось прочитать сохранённые данные. Они не тронуты, но изменения сейчас не сохраняются. Восстанови резервную копию в настройках." ]

        ( Persist, Just notice ) ->
            p [ class "notice" ] [ text notice ]

        ( Persist, Nothing ) ->
            text ""


viewPicker : Model -> PickState -> List (Html Msg)
viewPicker model state =
    let
        found =
            search state.query model.store.exercises

        played =
            Stats.lastPlayed (Store.allSegments model.now model.store)

        current =
            Session.exerciseId model.store.deck

        row e =
            div [ classList [ ( "row", True ), ( "current", current == Just e.id ) ] ]
                [ button [ class "row-main", onClick (Act (Choose (Just e.id))) ]
                    [ span [ class "row-name" ]
                        [ text e.name
                        , if e.video /= Nothing then
                            span [ class "tag" ] [ text "▶" ]

                          else
                            text ""
                        ]
                    , small []
                        [ text
                            (Dict.get e.id played
                                |> Maybe.map (Format.ago model.zone model.now)
                                |> Maybe.withDefault "ещё не играл"
                            )
                        ]
                    ]
                , div [ class "stepper" ]
                    [ button [ onClick (StartBump e.id -5), attribute "aria-label" "старт −5" ] [ text "−" ]
                    , span [] [ text (String.fromInt (Bpm.toInt e.startBpm)) ]
                    , button [ onClick (StartBump e.id 5), attribute "aria-label" "старт +5" ] [ text "+" ]
                    ]
                , a [ class "icon-btn small", href (routeHref (EditRoute e.id)), attribute "aria-label" "Изменить" ] [ icon Pencil ]
                ]

        byRecency =
            List.sortBy (\e -> Dict.get e.id played |> Maybe.withDefault 0 |> negate) found.matches
    in
    [ Html.form [ class "search", onSubmit (Act (SubmitSearch state.query)) ]
        [ input
            [ type_ "text"
            , placeholder "найти или добавить…"
            , value state.query
            , onInput QueryChanged
            , attribute "enterkeyhint" "done"
            ]
            []
        ]
    , if found.query /= "" && found.exact == Nothing then
        button [ class "add", onClick (Act (Create state.query)) ] [ text ("+ добавить «" ++ String.trim state.query ++ "»") ]

      else
        text ""
    , case state.suggested |> Maybe.andThen (Store.findExercise model.store) of
        Just e ->
            div [ class "suggestion" ] [ small [] [ text "может, это?" ], row e ]

        Nothing ->
            text ""
    , div [ class "list" ]
        ((if found.query == "" then
            [ div [ classList [ ( "row", True ), ( "current", current == Nothing ) ] ]
                [ button [ class "row-main", onClick (Act (Choose Nothing)) ]
                    [ span [ class "row-name" ] [ text "Без упражнения" ]
                    , small [] [ text "просто поиграть" ]
                    ]
                ]
            ]

          else
            []
         )
            ++ List.map row byRecency
        )
    , case model.store.exercises of
        [] ->
            p [ class "muted" ] [ text "Список пуст. Впиши название сверху — упражнение появится со стартом на текущем темпе." ]

        [ _ ] ->
            text ""

        _ ->
            button [ class "ghost wide", onClick (Act Suggest) ] [ text "🎲 подсказать" ]
    , p [ class "muted small" ] [ text "Число справа — стартовый темп, ± меняет его на 5." ]
    ]


viewEdit : EditState -> List (Html Msg)
viewEdit { form, confirmDelete, error } =
    [ label [ class "field" ]
        [ span [] [ text "Название" ]
        , input [ type_ "text", value form.name, onInput FormName ] []
        ]
    , div [ class "field" ]
        [ span [] [ text "Стартовый темп" ]
        , div [ class "bpm-edit" ]
            [ button [ class "key", onClick (FormBpmBump -5) ] [ text "−5" ]
            , input [ type_ "number", attribute "inputmode" "numeric", value form.bpm, onInput FormBpm ] []
            , button [ class "key", onClick (FormBpmBump 5) ] [ text "+5" ]
            ]
        ]
    , div [ class "field" ]
        [ span [] [ text "Доли" ]
        , div [ class "segmented" ]
            (List.map
                (\sub ->
                    button [ classList [ ( "on", form.subdivision == sub ) ], onClick (FormSubdivision sub) ]
                        [ text (Format.subdivision sub) ]
                )
                Exercise.subdivisions
            )
        ]
    , label [ class "field" ]
        [ span [] [ text "Видео (ссылка YouTube)" ]
        , input [ type_ "url", value form.video, onInput FormVideo, placeholder "https://youtu.be/…" ] []
        ]
    , if String.trim form.video /= "" && Exercise.youtubeId form.video == Nothing then
        p [ class "muted small" ] [ text "Это не YouTube: встроить не получится, будет просто ссылка." ]

      else
        text ""
    , viewMessage error
    , button [ class "primary wide", onClick SaveForm ] [ text "Сохранить" ]
    , if confirmDelete then
        button [ class "danger wide", onClick (Act (DeleteExercise form.id)) ] [ text "Точно удалить? История останется" ]

      else
        button [ class "ghost wide", onClick AskDelete ] [ text "Удалить" ]
    ]


viewMessage : Maybe String -> Html msg
viewMessage message =
    case message of
        Just m ->
            p [ class "notice" ] [ text m ]

        Nothing ->
            text ""


viewStats : Model -> List (Html Msg)
viewStats model =
    let
        segments =
            Store.allSegments model.now model.store

        byDay =
            Stats.msByDay model.zone segments

        today =
            Format.startOfDay model.zone model.now

        -- Noon avoids DST edges when stepping back whole days.
        day offset =
            today - offset * Format.dayMs + Format.dayMs // 2

        onDay offset =
            Dict.get (Stats.dayKey model.zone (day offset)) byDay |> Maybe.withDefault 0

        sumDays n =
            List.range 0 (n - 1) |> List.map onDay |> List.sum

        bars =
            List.range 0 13 |> List.reverse |> List.map (\o -> ( day o, onDay o ))

        peak =
            bars |> List.map Tuple.second |> List.maximum |> Maybe.withDefault 0 |> Basics.max 1

        recent =
            List.filter (\s -> s.startedAt >= today - 29 * Format.dayMs) segments

        totals =
            Stats.exerciseTotals recent

        rows =
            (Dict.toList totals.byExercise
                |> List.map
                    (\( id, ms ) ->
                        ( Store.findExercise model.store id |> Maybe.map .name |> Maybe.withDefault "(удалено)"
                        , ms
                        , Stats.latestTopBpm id segments
                        )
                    )
            )
                ++ (if totals.free > 0 then
                        [ ( "Без упражнения", totals.free, Nothing ) ]

                    else
                        []
                   )
                |> List.sortBy (\( _, ms, _ ) -> negate ms)
    in
    [ div [ class "tiles" ]
        [ tile "сегодня" (Format.duration (sumDays 1))
        , tile "7 дней" (Format.duration (sumDays 7))
        , tile "30 дней" (Format.duration (sumDays 30))
        ]
    , div [ class "bars" ]
        (List.map
            (\( t, ms ) ->
                div [ class "bar" ]
                    [ div [ class "bar-track" ]
                        [ div [ class "bar-fill", style "height" (String.fromFloat (100 * toFloat ms / toFloat peak) ++ "%") ] [] ]
                    , small [] [ text (Format.weekday model.zone t) ]
                    ]
            )
            bars
        )
    , h2 [] [ text "За 30 дней" ]
    , if List.isEmpty rows then
        p [ class "muted" ] [ text "Пока пусто." ]

      else
        div [ class "list" ]
            (List.map
                (\( name, ms, top ) ->
                    div [ class "row static" ]
                        [ span [ class "row-name" ] [ text name ]
                        , span [ class "row-meta" ]
                            [ text (Format.duration ms)
                            , case top of
                                Just t ->
                                    small [] [ text ("рекорд " ++ String.fromInt (Bpm.toInt t)) ]

                                Nothing ->
                                    text ""
                            ]
                        ]
                )
                rows
            )
    ]


tile : String -> String -> Html msg
tile label value =
    div [ class "tile" ] [ b [] [ text value ], small [] [ text label ] ]


viewSettings : Model -> SettingsState -> List (Html Msg)
viewSettings model state =
    let
        settings =
            model.store.settings

        tabLabel slot label =
            if slotFor model.systemDark == slot then
                label ++ " · сейчас"

            else
                label
    in
    [ h2 [] [ text "Тема" ]
    , div [ class "segmented" ]
        (List.map
            (\( slot, label ) ->
                button [ classList [ ( "on", state.themeSlot == slot ) ], onClick (PickThemeSlot slot) ]
                    [ text (tabLabel slot label) ]
            )
            [ ( Day, "☀ Днём" ), ( Night, "☾ Вечером" ) ]
        )
    , p [ class "muted small" ] [ text "Переключаются вместе со светлой и тёмной темой телефона." ]
    , div [ class "themes" ]
        (List.map
            (\t ->
                -- Each swatch carries its own theme, so it shows that palette.
                button
                    [ classList [ ( "swatch", True ), ( "on", slotTheme state.themeSlot settings == t ) ]
                    , attribute "data-theme" (Theme.id t)
                    , onClick (SetTheme state.themeSlot t)
                    ]
                    [ span [ class "swatch-dot" ] [], text (Theme.name t) ]
            )
            Theme.all
        )
    , h2 [] [ text ("Громкость клика · " ++ String.fromInt (round (settings.volume * 100)) ++ "%") ]
    , input
        [ type_ "range"
        , class "range"
        , Html.Attributes.min "0"
        , Html.Attributes.max "1"
        , step "0.05"
        , value (String.fromFloat settings.volume)
        , onInput (String.toFloat >> Maybe.withDefault 0.8 >> SetVolume)
        ]
        []
    , h2 [] [ text "Резервная копия" ]
    , p [ class "muted small" ] [ text "Всё хранится только на этом телефоне. Иногда делай экспорт." ]
    , div [ class "pair" ]
        [ button [ class "ghost", onClick Export ] [ text "Экспорт" ]
        , button [ class "ghost", onClick ImportFile ] [ text "Импорт файла" ]
        ]
    , h2 [] [ text "Вставить JSON" ]
    , p [ class "muted small" ]
        [ text "Список упражнений добавляется по названию: "
        , code [] [ text "{\"exercises\": [{\"name\": \"…\", \"bpm\": 90, \"video\": \"…\"}]}" ]
        , text ". Резервная копия заменяет всё."
        ]
    , textarea [ class "paste", value state.importText, onInput ImportTextChanged, rows 5 ] []
    , viewMessage state.message
    , button
        [ class "primary wide"
        , onClick (Act (ApplyImport state.importText))
        , disabled (String.trim state.importText == "")
        ]
        [ text "Импортировать" ]
    ]



-- ICONS


type Icon
    = Chart
    | Gear
    | Pencil
    | Cross


icon : Icon -> Html msg
icon kind =
    Svg.svg
        [ SA.viewBox "0 0 24 24"
        , SA.width "22"
        , SA.height "22"
        , SA.fill "none"
        , SA.stroke "currentColor"
        , SA.strokeWidth "2"
        , SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        ]
        (case kind of
            Chart ->
                [ Svg.path [ SA.d "M5 20V11M12 20V4M19 20v-6" ] [] ]

            Gear ->
                [ Svg.circle [ SA.cx "12", SA.cy "12", SA.r "3" ] []
                , Svg.path [ SA.d "M12 2v3M12 19v3M4.9 4.9l2.1 2.1M17 17l2.1 2.1M2 12h3M19 12h3M4.9 19.1L7 17M17 7l2.1-2.1" ] []
                ]

            Pencil ->
                [ Svg.path [ SA.d "M4 20h4L19 9l-4-4L4 16v4zM13.5 6.5l4 4" ] [] ]

            Cross ->
                [ Svg.path [ SA.d "M6 6l12 12M18 6L6 18" ] [] ]
        )



-- MAIN


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if Store.isRunning model.store then
            Time.every 250 (\_ -> Act Tick)

          else if model.store.live /= Nothing then
            -- Notice a session going idle while the app stays open.
            Time.every 60000 (\_ -> Act Tick)

          else
            Sub.none
        , Browser.Events.onVisibilityChange VisibilityChanged
        , systemDark SystemDarkChanged
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
