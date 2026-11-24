module StoreTest exposing (suite)

import Bpm
import Exercise exposing (Subdivision(..))
import Expect
import Json.Decode as D
import Json.Encode as E
import Session
import Store exposing (Store)
import Test exposing (Test, describe, test)
import Theme


{-| A real v1 export (exercises and presets trimmed, sessions verbatim).
-}
v1Export : String
v1Export =
    """{
  "version": 1,
  "exercises": [
    { "id": "song", "name": "Песня", "startBpm": 60, "subdivision": 1, "metronome": true, "notes": "", "archived": false, "parentId": null },
    { "id": "song-crying-fire", "name": "Avatar — Crying Fire", "startBpm": 88, "subdivision": 1, "metronome": true, "notes": "", "archived": false, "parentId": "song" }
  ],
  "presets": [
    { "id": "technique", "name": "Техника", "items": [ { "exerciseId": "legato-workout", "minutes": 3 } ] },
    { "id": "daily", "name": "Ежедневная", "items": [ { "choices": [ { "exerciseId": "sweep", "minutes": 10, "random": true } ] } ] }
  ],
  "sessions": [
    { "id": "s1793383541586", "startedAt": 1793383541586, "endedAt": 1793385839540, "presetId": "p1793268016255",
      "segments": [
        { "exerciseId": "legato-workout", "startedAt": 1793383541586, "durationMs": 247349, "startBpm": 70, "maxBpm": 70 },
        { "exerciseId": "sequence-fours", "startedAt": 1793383875761, "durationMs": 459804, "startBpm": 80, "maxBpm": 100 },
        { "exerciseId": "lick-blues", "startedAt": 1793384355247, "durationMs": 608084, "startBpm": 90, "maxBpm": 95 },
        { "exerciseId": "rhythm-blues", "startedAt": 1793385293984, "durationMs": 259511, "startBpm": 90, "maxBpm": 90 }
      ] },
    { "id": "s1793288358788", "startedAt": 1793288358788, "endedAt": 1793290723692, "presetId": "p1793268016256",
      "segments": [
        { "exerciseId": "legato-workout", "startedAt": 1793288358788, "durationMs": 224573, "startBpm": 70, "maxBpm": 70 },
        { "exerciseId": "e1793266883856", "startedAt": 1793288662764, "durationMs": 463669, "startBpm": 60, "maxBpm": 60 },
        { "exerciseId": "sweep-e", "startedAt": 1793289205036, "durationMs": 615365, "startBpm": 80, "maxBpm": 100 },
        { "exerciseId": "song-crying-fire", "startedAt": 1793289918814, "durationMs": 763026, "startBpm": 88, "maxBpm": 93 }
      ] },
    { "id": "s1793218028152", "startedAt": 1793218028152, "endedAt": 1793221109152, "presetId": "technique",
      "segments": [
        { "exerciseId": "legato-workout", "startedAt": 1793218028152, "durationMs": 261184, "startBpm": 80, "maxBpm": 80 },
        { "exerciseId": "sequence-fours", "startedAt": 1793218360403, "durationMs": 440600, "startBpm": 90, "maxBpm": 90 },
        { "exerciseId": "sequence-slash", "startedAt": 1793219601916, "durationMs": 612332, "startBpm": 100, "maxBpm": 120 },
        { "exerciseId": "sweep-e", "startedAt": 1793220311918, "durationMs": 607705, "startBpm": 90, "maxBpm": 90 }
      ] }
  ],
  "live": LIVE,
  "settings": { "volume": 0.65 }
}"""


v1ExportTotal : Int
v1ExportTotal =
    5563202


{-| v2 data as written by the first v2 build (pinned: must keep decoding).
-}
v2Saved : String
v2Saved =
    """{"version":2,
 "exercises":[
  {"id":"e1","name":"Avatar — Crying Fire","startBpm":88,"subdivision":1,"video":"https://youtu.be/dQw4w9WgXcQ"},
  {"id":"e2","name":"Slash's sequence","startBpm":90,"subdivision":2,"video":""}],
 "sessions":[{"id":"s1","startedAt":1000,"endedAt":2801000,"segments":[
  {"exerciseId":"e2","startedAt":1000,"durationMs":900000,"startBpm":90,"maxBpm":110},
  {"exerciseId":null,"startedAt":901000,"durationMs":600000,"startBpm":0,"maxBpm":0}]}],
 "live":{"id":"s2","startedAt":5000000,"heartbeat":5600000,
  "segments":[{"exerciseId":"e2","startedAt":5000000,"durationMs":180000,"startBpm":90,"maxBpm":95}],
  "open":{"exerciseId":"e1","startedAt":5300000,"startBpm":88,"maxBpm":0,"accumulatedMs":252000,"runningSince":null,"bpmMark":0}},
 "deck":{"exerciseId":"e1","bpm":93,"subdivision":1,"clickOn":true},
 "settings":{"volume":0.8,"theme":"forest"}}"""


withLive : String -> String
withLive live =
    String.replace "LIVE" live v1Export


{-| A v1 session left running: one closed segment of 60 s, then 30 s
accumulated plus 15 s running up to the heartbeat.
-}
v1Live : String
v1Live =
    """{ "id": "s2000000000000", "startedAt": 2000000000000, "presetId": null,
  "segments": [ { "exerciseId": "free", "startedAt": 2000000000000, "durationMs": 60000, "startBpm": 100, "maxBpm": 0 } ],
  "active": { "exerciseId": "sweep-e", "bpm": 90, "subdivision": 2, "metronomeOn": true, "startBpm": 90, "maxBpm": 0,
              "startedAt": 2000000070000, "accumulatedMs": 30000, "runningSince": 2000000100000, "bpmMark": 0 },
  "chimed": [], "heartbeat": 2000000115000,
  "plan": [ { "exerciseId": "sweep-e", "minutes": 10, "choices": [] } ] }"""


allSegments : Store -> List Session.Segment
allSegments store =
    List.concatMap .segments store.sessions


totalMs : List Session.Segment -> Int
totalMs =
    List.map .durationMs >> List.sum


migrate : String -> Result D.Error Store
migrate =
    D.decodeString Store.legacyDecoder


decodeV2 : String -> Result D.Error Store
decodeV2 =
    D.decodeString Store.decoder


{-| Two exercises, "a" (start 90, eighths) on the deck, nothing played,
click off, triplets for free play.
-}
fresh : Store
fresh =
    let
        base =
            Store.empty
    in
    Store.switchTo 0
        (Just "a")
        { base
            | exercises =
                [ { id = "a", name = "Alpha", startBpm = Bpm.fromInt 90, subdivision = Eighth, video = Nothing }
                , { id = "b", name = "Beta", startBpm = Bpm.fromInt 70, subdivision = Quarter, video = Nothing }
                ]
            , deck = Session.newDeck |> Session.toggleClick |> Session.setFreeSubdivision Triplet
        }


tempo : Store -> Int
tempo =
    .deck >> Session.bpm >> Bpm.toInt


sec : Int -> Int
sec n =
    n * 1000


suite : Test
suite =
    describe "Store"
        [ describe "v1 migration"
            [ test "keeps every millisecond of practice" <|
                \_ ->
                    migrate (withLive "null")
                        |> Result.map (allSegments >> totalMs)
                        |> Expect.equal (Ok v1ExportTotal)
            , test "turns everything into free play with no top BPM" <|
                \_ ->
                    migrate (withLive "null")
                        |> Result.map (allSegments >> List.all (\s -> s.exerciseId == Nothing && s.topBpm == Nothing))
                        |> Expect.equal (Ok True)
            , test "drops exercises, keeps volume, sorts newest first" <|
                \_ ->
                    migrate (withLive "null")
                        |> Result.map (\s -> ( s.exercises, s.settings.volume, List.map .id s.sessions ))
                        |> Expect.equal (Ok ( [], 0.65, [ "s1793383541586", "s1793288358788", "s1793218028152" ] ))
            , test "keeps time from a session left running, up to its heartbeat" <|
                \_ ->
                    migrate (withLive v1Live)
                        |> Result.map (allSegments >> totalMs)
                        |> Expect.equal (Ok (v1ExportTotal + 60000 + 45000))
            , test "survives unexpected shapes" <|
                \_ ->
                    migrate """{ "sessions": [ { "segments": [ { "startedAt": 1, "durationMs": 5000 }, { "bad": true } ] }, 42 ], "presets": "??" }"""
                        |> Result.map (allSegments >> List.map .durationMs)
                        |> Expect.equal (Ok [ 5000 ])
            , test "a v1 backup adds its history once, however often imported" <|
                \_ ->
                    case Store.decodeImport (withLive "null") of
                        Ok imp ->
                            Store.empty
                                |> Store.applyImport 0 imp
                                |> Store.applyImport 0 imp
                                |> allSegments
                                |> totalMs
                                |> Expect.equal v1ExportTotal

                        Err e ->
                            Expect.fail e
            ]
        , describe "v2 format"
            [ test "data from the first v2 build still reads the same" <|
                \_ ->
                    decodeV2 v2Saved
                        |> Result.map
                            (\s ->
                                ( ( totalMs (Store.allSegments 0 s), Session.exerciseId s.deck, tempo s )
                                , ( s.settings.theme, List.map .video s.exercises, List.map .subdivision s.exercises )
                                , allSegments s |> List.map (.topBpm >> Maybe.map Bpm.toInt)
                                )
                            )
                        |> Expect.equal
                            (Ok
                                ( ( 900000 + 600000 + 180000 + 252000, Just "e1", 93 )
                                , ( Theme.Forest, [ Just "https://youtu.be/dQw4w9WgXcQ", Nothing ], [ Quarter, Eighth ] )
                                , [ Just 110, Nothing ]
                                )
                            )
            , test "round-trips" <|
                \_ ->
                    let
                        played =
                            fresh |> Store.togglePlay 0 |> Store.togglePlay (sec 30)
                    in
                    E.encode 0 (Store.encode played)
                        |> decodeV2
                        |> Expect.equal (Ok played)
            , test "clamps or defaults bad values instead of failing" <|
                \_ ->
                    decodeV2
                        """{"version":2,"exercises":[{"id":"x","name":"X","startBpm":0,"subdivision":0,"video":""}],
                            "sessions":[],"live":null,"deck":{"exerciseId":"gone","bpm":999,"subdivision":7,"clickOn":true},
                            "settings":{"volume":5,"theme":"neon"}}"""
                        |> Result.map
                            (\s ->
                                ( List.map (\e -> ( Bpm.toInt e.startBpm, e.subdivision )) s.exercises
                                , ( Session.exerciseId s.deck, tempo s, Session.freeSubdivision s.deck )
                                , ( s.settings.volume, s.settings.theme )
                                )
                            )
                        |> Expect.equal (Ok ( [ ( 20, Quarter ) ], ( Nothing, 400, Quarter ), ( 1, Theme.Lamp ) ))
            , test "a v2 backup imports as a full replacement" <|
                \_ ->
                    case Store.decodeImport (E.encode 0 (Store.encode fresh)) of
                        Ok (Store.FullBackup s) ->
                            Expect.equal fresh s

                        _ ->
                            Expect.fail "expected FullBackup"
            ]
        , describe "deck"
            [ test "switching keeps the time under the exercise it was played on" <|
                \_ ->
                    fresh
                        |> Store.togglePlay 0
                        |> Store.switchTo (sec 40) (Just "b")
                        |> Store.togglePlay (sec 40)
                        |> Store.allSegments (sec 50)
                        |> List.map (\s -> ( s.exerciseId, s.durationMs ))
                        |> Expect.equal [ ( Just "a", sec 40 ), ( Just "b", sec 10 ) ]
            , test "switching puts the deck at the new exercise's start BPM, paused" <|
                \_ ->
                    fresh
                        |> Store.togglePlay 0
                        |> Store.switchTo (sec 5) (Just "b")
                        |> (\s -> ( tempo s, Store.isRunning s ))
                        |> Expect.equal ( 70, False )
            , test "an unknown id switches to free play" <|
                \_ ->
                    Session.exerciseId (Store.switchTo 0 (Just "nope") fresh).deck
                        |> Expect.equal Nothing
            , test "an unplayed deck follows its start BPM; a played one keeps its tempo" <|
                \_ ->
                    let
                        bumpStart =
                            Store.updateExercise "a" (\e -> { e | startBpm = Bpm.add 5 e.startBpm })
                    in
                    ( tempo (bumpStart fresh)
                    , tempo (fresh |> Store.togglePlay 0 |> Store.bump 1 3 |> bumpStart)
                    )
                        |> Expect.equal ( 95, 93 )
            , test "other edits never reset the tempo, even before playing" <|
                \_ ->
                    let
                        bumped =
                            Store.bump 0 3 fresh
                    in
                    [ Store.setSubdivision Sixteenth bumped
                    , Store.updateExercise "b" (\e -> { e | startBpm = Bpm.add 5 e.startBpm }) bumped
                    , Store.updateExercise "a" (\e -> { e | name = "Renamed" }) bumped
                    ]
                        |> List.map tempo
                        |> Expect.equal [ 93, 93, 93 ]
            , test "subdivision belongs to the exercise, or to the deck in free play" <|
                \_ ->
                    let
                        onExercise =
                            Store.setSubdivision Sixteenth fresh

                        free =
                            fresh |> Store.switchTo 0 Nothing |> Store.setSubdivision Eighth
                    in
                    ( ( Store.subdivision onExercise, List.map .subdivision onExercise.exercises )
                    , ( Store.subdivision free, List.map .subdivision free.exercises )
                    )
                        |> Expect.equal
                            ( ( Sixteenth, [ Sixteenth, Quarter ] )
                            , ( Eighth, [ Eighth, Quarter ] )
                            )
            , test "deleting the exercise on the deck closes its time and goes to free play" <|
                \_ ->
                    fresh
                        |> Store.togglePlay 0
                        |> Store.deleteExercise (sec 20) "a"
                        |> (\s -> ( Session.exerciseId s.deck, List.map .id s.exercises, Store.allSegments (sec 99) s |> List.map .durationMs ))
                        |> Expect.equal ( Nothing, [ "b" ], [ sec 20 ] )
            , test "a session paused for 30 minutes goes into history" <|
                \_ ->
                    fresh
                        |> Store.togglePlay 0
                        |> Store.togglePlay (sec 60)
                        |> Store.tick (sec 61 + Session.idleLimitMs)
                        |> (\s -> ( s.live == Nothing, List.map (\x -> ( totalMs x.segments, x.endedAt )) s.sessions ))
                        |> Expect.equal ( True, [ ( sec 60, sec 60 ) ] )
            ]
        , test "an exercise list merges by name" <|
            \_ ->
                case Store.decodeImport """{ "exercises": [ { "name": "alpha", "bpm": 100, "video": "v" }, { "name": "Новое", "subdivision": 3 } ] }""" of
                    Ok imp ->
                        (Store.applyImport 7 imp fresh).exercises
                            |> List.map (\e -> ( e.name, Bpm.toInt e.startBpm, ( e.subdivision, e.video ) ))
                            |> Expect.equal
                                [ ( "Alpha", 100, ( Eighth, Just "v" ) )
                                , ( "Beta", 70, ( Quarter, Nothing ) )
                                , ( "Новое", 80, ( Triplet, Nothing ) )
                                ]

                    Err e ->
                        Expect.fail e
        ]
