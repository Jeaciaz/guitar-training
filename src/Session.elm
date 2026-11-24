module Session exposing
    ( Deck
    , Live
    , Segment
    , Session
    , bpm
    , clickOn
    , countInRemaining
    , deckDecoder
    , encodeDeck
    , encodeLive
    , encodeSession
    , exerciseId
    , exerciseTime
    , finish
    , followStart
    , freeSubdivision
    , idleLimitMs
    , isIdle
    , isRunning
    , liveDecoder
    , newDeck
    , pause
    , play
    , restore
    , segmentsAsOf
    , sessionDecoder
    , setFreeSubdivision
    , setTempo
    , switch
    , toggleClick
    , total
    , touch
    )

{-| Practice time accounting. Times are epoch millis.

A session starts implicitly on the first play and holds closed segments plus
at most one open segment. The open segment always belongs to whatever is on
the deck and carries no exercise of its own. `Deck` is opaque so its exercise
can only change through `switch`, which closes that segment first, and its
tempo only through `setTempo`, which credits the tempo being left.

-}

import Bpm exposing (Bpm)
import Exercise exposing (Subdivision(..))
import Json.Decode as D exposing (Decoder)
import Json.Encode as E



-- TYPES


{-| What the metronome screen shows: an exercise or free play, its tempo,
whether the click is on, and the subdivision used in free play (an
exercise brings its own).
-}
type Deck
    = Deck
        { exerciseId : Maybe String
        , bpm : Bpm
        , freeSubdivision : Subdivision
        , clickOn : Bool
        }


{-| One continuous stay on one exercise (`Nothing` is free play). `topBpm`
is the highest tempo held for `settleMs`, if any.
-}
type alias Segment =
    { exerciseId : Maybe String
    , startedAt : Int
    , durationMs : Int
    , topBpm : Maybe Bpm
    }


type alias Session =
    { id : String
    , startedAt : Int
    , endedAt : Int
    , segments : List Segment
    }


type Live
    = Live
        { id : String
        , startedAt : Int
        , segments : List Segment
        , open : Maybe Open
        , heartbeat : Int
        }


{-| `bpmMark` is the elapsed time when the current tempo was set; a tempo
only counts towards `topBpm` once played for `settleMs`.
-}
type alias Open =
    { startedAt : Int
    , topBpm : Maybe Bpm
    , accumulatedMs : Int
    , clock : Clock
    , bpmMark : Int
    }


{-| While a count-in plays, `RunningSince` lies in the future: time only
accrues once it has passed.
-}
type Clock
    = Paused
    | RunningSince Int



-- CONSTANTS


idleLimitMs : Int
idleLimitMs =
    30 * 60 * 1000


settleMs : Int
settleMs =
    10 * 1000



-- DECK


{-| Free play at 80, quarters, click on.
-}
newDeck : Deck
newDeck =
    Deck { exerciseId = Nothing, bpm = Bpm.fromInt 80, freeSubdivision = Quarter, clickOn = True }


exerciseId : Deck -> Maybe String
exerciseId (Deck d) =
    d.exerciseId


bpm : Deck -> Bpm
bpm (Deck d) =
    d.bpm


clickOn : Deck -> Bool
clickOn (Deck d) =
    d.clickOn


freeSubdivision : Deck -> Subdivision
freeSubdivision (Deck d) =
    d.freeSubdivision


toggleClick : Deck -> Deck
toggleClick (Deck d) =
    Deck { d | clickOn = not d.clickOn }


setFreeSubdivision : Subdivision -> Deck -> Deck
setFreeSubdivision sub (Deck d) =
    Deck { d | freeSubdivision = sub }


{-| Change the tempo, crediting the one being left if it was held long enough.
-}
setTempo : Int -> Bpm -> Deck -> Maybe Live -> ( Deck, Maybe Live )
setTempo now tempo ((Deck d) as deck) live =
    let
        markTempo o =
            let
                settled =
                    settle now deck o
            in
            { settled | bpmMark = elapsed now o }
    in
    ( Deck { d | bpm = tempo }, Maybe.map (mapOpen markTempo) live )


{-| Put an exercise (id and start BPM) or free play (`Nothing`, keeping the
tempo) on the deck, paused. The open segment is closed first, under the
exercise it was played on. Under a second isn't kept.
-}
switch : Int -> Maybe ( String, Bpm ) -> Deck -> Maybe Live -> ( Deck, Maybe Live )
switch now target ((Deck d) as deck) live =
    if Maybe.map Tuple.first target == d.exerciseId then
        ( deck, live )

    else
        ( Deck
            { d
                | exerciseId = Maybe.map Tuple.first target
                , bpm = target |> Maybe.map Tuple.second |> Maybe.withDefault d.bpm
            }
        , Maybe.map (leave now deck) live
        )


{-| The deck's exercise has a new start BPM: take it while nothing has been
played on it yet.
-}
followStart : Bpm -> Deck -> Maybe Live -> Deck
followStart start (Deck d) live =
    let
        played =
            live |> Maybe.map hasOpenSegment |> Maybe.withDefault False
    in
    if played then
        Deck d

    else
        Deck { d | bpm = start }



-- QUERIES


isRunning : Live -> Bool
isRunning (Live l) =
    case l.open of
        Just o ->
            o.clock /= Paused

        Nothing ->
            False


hasOpenSegment : Live -> Bool
hasOpenSegment (Live l) =
    l.open /= Nothing


countInRemaining : Int -> Live -> Int
countInRemaining now (Live l) =
    case Maybe.map .clock l.open of
        Just (RunningSince since) ->
            max 0 (since - now)

        _ ->
            0


isIdle : Int -> Live -> Bool
isIdle now ((Live l) as live) =
    not (isRunning live) && now - l.heartbeat > idleLimitMs


elapsed : Int -> Open -> Int
elapsed now o =
    case o.clock of
        RunningSince since ->
            o.accumulatedMs + max 0 (now - since)

        Paused ->
            o.accumulatedMs


{-| Closed segments plus the open one as of `now`.
-}
segmentsAsOf : Int -> Deck -> Live -> List Segment
segmentsAsOf now deck (Live l) =
    case l.open of
        Just o ->
            l.segments ++ [ toSegment now deck o ]

        Nothing ->
            l.segments


exerciseTime : Int -> Deck -> Live -> Int
exerciseTime now deck live =
    segmentsAsOf now deck live
        |> List.filter (\s -> s.exerciseId == exerciseId deck)
        |> List.map .durationMs
        |> List.sum


total : Int -> Deck -> Live -> Int
total now deck live =
    segmentsAsOf now deck live |> List.map .durationMs |> List.sum



-- TRANSITIONS


{-| Credit the deck's tempo if it has been held since `bpmMark`.
-}
settle : Int -> Deck -> Open -> Open
settle now (Deck d) o =
    if elapsed now o - o.bpmMark >= settleMs then
        { o | topBpm = Just (Maybe.withDefault d.bpm (Maybe.map (Bpm.max d.bpm) o.topBpm)) }

    else
        o


mapOpen : (Open -> Open) -> Live -> Live
mapOpen f (Live l) =
    Live { l | open = Maybe.map f l.open }


toSegment : Int -> Deck -> Open -> Segment
toSegment now deck o =
    { exerciseId = exerciseId deck
    , startedAt = o.startedAt
    , durationMs = elapsed now o
    , topBpm = (settle now deck o).topBpm
    }


{-| Start or resume the deck, opening a session if there is none. A one-bar
count-in plays first when the click is on.
-}
play : Int -> Deck -> Maybe Live -> Live
play now (Deck d) maybeLive =
    let
        l =
            case maybeLive of
                Just (Live existing) ->
                    existing

                Nothing ->
                    { id = "s" ++ String.fromInt now, startedAt = now, segments = [], open = Nothing, heartbeat = now }

        countIn =
            if d.clickOn then
                4 * 60000 // Bpm.toInt d.bpm

            else
                0

        o =
            Maybe.withDefault
                { startedAt = now, topBpm = Nothing, accumulatedMs = 0, clock = Paused, bpmMark = 0 }
                l.open
    in
    if o.clock == Paused then
        Live { l | open = Just { o | clock = RunningSince (now + countIn) }, heartbeat = now }

    else
        Live l


pause : Int -> Deck -> Live -> Live
pause now deck (Live l) =
    let
        stop o =
            let
                settled =
                    settle now deck o
            in
            { settled | accumulatedMs = elapsed now o, clock = Paused }
    in
    Live { l | open = Maybe.map stop l.open, heartbeat = now }


{-| Close the deck's segment (time up to `now`), leaving nothing open.
-}
leave : Int -> Deck -> Live -> Live
leave now deck ((Live l) as live) =
    case l.open of
        Just o ->
            let
                segment =
                    toSegment now deck o
            in
            Live
                { l
                    | open = Nothing
                    , heartbeat =
                        if isRunning live then
                            now

                        else
                            l.heartbeat
                    , segments =
                        if segment.durationMs >= 1000 then
                            l.segments ++ [ segment ]

                        else
                            l.segments
                }

        Nothing ->
            live


{-| Record that the app was alive and playing (at most every 5 s, so a
running session doesn't rewrite storage on every tick).
-}
touch : Int -> Deck -> Live -> Live
touch now deck ((Live l) as live) =
    if isRunning live && now - l.heartbeat >= 5000 then
        Live { l | heartbeat = now, open = Maybe.map (settle now deck) l.open }

    else
        live


{-| After a reload: a segment left running is paused at the last heartbeat,
so a killed app never inflates practice time.
-}
restore : Live -> Live
restore (Live l) =
    Live
        { l
            | open =
                Maybe.map
                    (\o -> { o | accumulatedMs = elapsed l.heartbeat o, clock = Paused })
                    l.open
        }


{-| Close the session: at `now` if it is running, else when it was paused.
`Nothing` when nothing was played.
-}
finish : Int -> Deck -> Live -> Maybe Session
finish now deck live =
    let
        (Live closed) =
            leave now deck live
    in
    if List.isEmpty closed.segments then
        Nothing

    else
        Just { id = closed.id, startedAt = closed.startedAt, endedAt = closed.heartbeat, segments = closed.segments }



-- JSON


encodeDeck : Deck -> E.Value
encodeDeck (Deck d) =
    E.object
        [ ( "exerciseId", maybe E.string d.exerciseId )
        , ( "bpm", Bpm.encode d.bpm )
        , ( "subdivision", E.int (Exercise.perBeat d.freeSubdivision) )
        , ( "clickOn", E.bool d.clickOn )
        ]


{-| The exercise id isn't checked here; `Store.decoder` switches a deck
naming a missing exercise to free play.
-}
deckDecoder : Decoder Deck
deckDecoder =
    D.map4 (\id tempo sub click -> Deck { exerciseId = id, bpm = tempo, freeSubdivision = sub, clickOn = click })
        (D.field "exerciseId" (D.nullable D.string))
        (D.field "bpm" Bpm.decoder)
        (D.field "subdivision" Exercise.subdivisionDecoder)
        (D.field "clickOn" D.bool)


{-| `maxBpm` 0 means no top tempo (as in history migrated from v1).
-}
encodeSegment : Segment -> E.Value
encodeSegment s =
    E.object
        [ ( "exerciseId", maybe E.string s.exerciseId )
        , ( "startedAt", E.int s.startedAt )
        , ( "durationMs", E.int s.durationMs )
        , ( "maxBpm", encodeTop s.topBpm )
        ]


encodeTop : Maybe Bpm -> E.Value
encodeTop =
    Maybe.map Bpm.encode >> Maybe.withDefault (E.int 0)


segmentDecoder : Decoder Segment
segmentDecoder =
    D.map4 Segment
        (D.field "exerciseId" (D.nullable D.string))
        (D.field "startedAt" D.int)
        (D.field "durationMs" D.int)
        (D.field "maxBpm" topBpmDecoder)


topBpmDecoder : Decoder (Maybe Bpm)
topBpmDecoder =
    D.map
        (\n ->
            if n > 0 then
                Just (Bpm.fromInt n)

            else
                Nothing
        )
        D.int


encodeSession : Session -> E.Value
encodeSession s =
    E.object
        [ ( "id", E.string s.id )
        , ( "startedAt", E.int s.startedAt )
        , ( "endedAt", E.int s.endedAt )
        , ( "segments", E.list encodeSegment s.segments )
        ]


sessionDecoder : Decoder Session
sessionDecoder =
    D.map4 Session
        (D.field "id" D.string)
        (D.field "startedAt" D.int)
        (D.field "endedAt" D.int)
        (D.field "segments" (D.list segmentDecoder))


encodeLive : Live -> E.Value
encodeLive (Live l) =
    E.object
        [ ( "id", E.string l.id )
        , ( "startedAt", E.int l.startedAt )
        , ( "segments", E.list encodeSegment l.segments )
        , ( "open", maybe encodeOpen l.open )
        , ( "heartbeat", E.int l.heartbeat )
        ]


encodeOpen : Open -> E.Value
encodeOpen o =
    E.object
        [ ( "startedAt", E.int o.startedAt )
        , ( "maxBpm", encodeTop o.topBpm )
        , ( "accumulatedMs", E.int o.accumulatedMs )
        , ( "runningSince"
          , case o.clock of
                RunningSince since ->
                    E.int since

                Paused ->
                    E.null
          )
        , ( "bpmMark", E.int o.bpmMark )
        ]


liveDecoder : Decoder Live
liveDecoder =
    D.map5 (\id startedAt segments open heartbeat -> Live { id = id, startedAt = startedAt, segments = segments, open = open, heartbeat = heartbeat })
        (D.field "id" D.string)
        (D.field "startedAt" D.int)
        (D.field "segments" (D.list segmentDecoder))
        (D.field "open"
            (D.nullable
                (D.map5 Open
                    (D.field "startedAt" D.int)
                    (D.field "maxBpm" topBpmDecoder)
                    (D.field "accumulatedMs" D.int)
                    (D.field "runningSince" (D.nullable D.int)
                        |> D.map (Maybe.map RunningSince >> Maybe.withDefault Paused)
                    )
                    (D.field "bpmMark" D.int)
                )
            )
        )
        (D.field "heartbeat" D.int)


maybe : (a -> E.Value) -> Maybe a -> E.Value
maybe f =
    Maybe.map f >> Maybe.withDefault E.null
