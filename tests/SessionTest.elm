module SessionTest exposing (suite)

import Bpm
import Exercise
import Expect
import Session exposing (Deck, Live)
import Test exposing (Test, describe, test)


{-| Exercise "e1" at `bpm`, click on or off.
-}
deckAt : Int -> Bool -> Deck
deckAt bpm click =
    let
        base =
            if click then
                Session.newDeck

            else
                Session.toggleClick Session.newDeck
    in
    Session.switch 0 (Just ( "e1", Bpm.fromInt bpm )) base Nothing |> Tuple.first


deck : Deck
deck =
    deckAt 60 True


silent : Deck
silent =
    deckAt 60 False


sec : Int -> Int
sec n =
    n * 1000


suite : Test
suite =
    describe "Session"
        [ describe "play / pause"
            [ test "the click gets a one-bar count-in that isn't counted" <|
                \_ ->
                    let
                        live =
                            Session.play 0 deck Nothing
                    in
                    ( Session.countInRemaining 0 live, Session.total (sec 4) deck live, Session.total (sec 10) deck live )
                        |> Expect.equal ( sec 4, 0, sec 6 )
            , test "without the click time runs at once" <|
                \_ ->
                    Session.play 0 silent Nothing
                        |> Session.total (sec 10) silent
                        |> Expect.equal (sec 10)
            , test "paused time doesn't count" <|
                \_ ->
                    Session.play 0 silent Nothing
                        |> Session.pause (sec 10) silent
                        |> Just
                        |> Session.play (sec 100) silent
                        |> Session.total (sec 105) silent
                        |> Expect.equal (sec 15)
            , test "a tempo counts as top only once held for 10 s" <|
                \_ ->
                    let
                        retempo now bpm ( d, l ) =
                            Session.setTempo now (Bpm.fromInt bpm) d l
                    in
                    -- 60 for 20 s, 120 for 5 s (mis-tap), then 70 for 12 s
                    ( silent, Just (Session.play 0 silent Nothing) )
                        |> retempo (sec 20) 120
                        |> retempo (sec 25) 70
                        |> (\( d, l ) -> Maybe.andThen (Session.finish (sec 37) d) l)
                        |> Maybe.map (.segments >> List.map (.topBpm >> Maybe.map Bpm.toInt))
                        |> Expect.equal (Just [ Just 70 ])
            , test "nothing held for 10 s means no top tempo" <|
                \_ ->
                    Session.play 0 silent Nothing
                        |> Session.finish (sec 8) silent
                        |> Maybe.map (.segments >> List.map .topBpm)
                        |> Expect.equal (Just [ Nothing ])
            ]
        , describe "segments"
            [ test "switching closes the open segment under the exercise it was played on" <|
                \_ ->
                    let
                        ( free, live ) =
                            Session.switch (sec 30) Nothing silent (Just (Session.play 0 silent Nothing))
                    in
                    Session.play (sec 30) free live
                        |> Session.segmentsAsOf (sec 50) free
                        |> List.map (\s -> ( s.exerciseId, s.durationMs ))
                        |> Expect.equal [ ( Just "e1", sec 30 ), ( Nothing, sec 20 ) ]
            , test "switching to free play keeps the tempo; to an exercise takes its start" <|
                \_ ->
                    let
                        ( free, _ ) =
                            Session.switch 0 Nothing (deckAt 93 False) Nothing

                        ( other, _ ) =
                            Session.switch 0 (Just ( "e2", Bpm.fromInt 70 )) free Nothing
                    in
                    ( Bpm.toInt (Session.bpm free), Bpm.toInt (Session.bpm other), Session.exerciseId other )
                        |> Expect.equal ( 93, 70, Just "e2" )
            , test "the start BPM is only followed before anything is played" <|
                \_ ->
                    let
                        played =
                            Just (Session.play 0 silent Nothing)
                    in
                    ( Session.followStart (Bpm.fromInt 75) silent Nothing |> Session.bpm |> Bpm.toInt
                    , Session.followStart (Bpm.fromInt 75) silent played |> Session.bpm |> Bpm.toInt
                    )
                        |> Expect.equal ( 75, 60 )
            , test "under a second isn't recorded" <|
                \_ ->
                    Session.play 0 silent Nothing
                        |> Session.finish 500 silent
                        |> Expect.equal Nothing
            , test "a reload pauses at the last heartbeat" <|
                \_ ->
                    Session.play 0 silent Nothing
                        |> Session.touch (sec 40) silent
                        |> Session.restore
                        |> Session.total (sec 999) silent
                        |> Expect.equal (sec 40)
            , test "idle only after 30 paused minutes" <|
                \_ ->
                    let
                        live : Live
                        live =
                            Session.play 0 silent Nothing |> Session.pause (sec 60) silent
                    in
                    ( Session.isIdle (sec 60 + Session.idleLimitMs) live, Session.isIdle (sec 61 + Session.idleLimitMs) live )
                        |> Expect.equal ( False, True )
            ]
        , describe "youtubeId"
            [ test "understands the usual link shapes" <|
                \_ ->
                    [ "https://www.youtube.com/watch?v=dQw4w9WgXcQ&t=42"
                    , "https://youtu.be/dQw4w9WgXcQ?si=xyz"
                    , "https://m.youtube.com/shorts/dQw4w9WgXcQ"
                    , "https://youtube.com/embed/dQw4w9WgXcQ"
                    ]
                        |> List.map Exercise.youtubeId
                        |> Expect.equal (List.repeat 4 (Just "dQw4w9WgXcQ"))
            , test "rejects other links" <|
                \_ ->
                    [ "https://vimeo.com/123", "not a url", "https://www.youtube.com/watch?v=<script>" ]
                        |> List.map Exercise.youtubeId
                        |> Expect.equal [ Nothing, Nothing, Nothing ]
            ]
        ]
