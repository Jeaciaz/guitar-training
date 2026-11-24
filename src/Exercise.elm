module Exercise exposing
    ( Exercise
    , Subdivision(..)
    , decoder
    , encode
    , nextSubdivision
    , perBeat
    , subdivisionDecoder
    , subdivisions
    , youtubeId
    )

{-| An exercise from the list, and how the click divides its beat.
-}

import Bpm exposing (Bpm)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Url


type alias Exercise =
    { id : String
    , name : String
    , startBpm : Bpm
    , subdivision : Subdivision
    , video : Maybe String
    }


type Subdivision
    = Quarter
    | Eighth
    | Triplet
    | Sixteenth


subdivisions : List Subdivision
subdivisions =
    [ Quarter, Eighth, Triplet, Sixteenth ]


{-| Clicks per beat.
-}
perBeat : Subdivision -> Int
perBeat sub =
    case sub of
        Quarter ->
            1

        Eighth ->
            2

        Triplet ->
            3

        Sixteenth ->
            4


nextSubdivision : Subdivision -> Subdivision
nextSubdivision sub =
    case sub of
        Quarter ->
            Eighth

        Eighth ->
            Triplet

        Triplet ->
            Sixteenth

        Sixteenth ->
            Quarter



-- JSON


{-| Stored as clicks per beat; anything unknown reads as quarters.
-}
subdivisionDecoder : Decoder Subdivision
subdivisionDecoder =
    D.map
        (\n ->
            subdivisions
                |> List.filter (\s -> perBeat s == n)
                |> List.head
                |> Maybe.withDefault Quarter
        )
        D.int


encode : Exercise -> E.Value
encode e =
    E.object
        [ ( "id", E.string e.id )
        , ( "name", E.string e.name )
        , ( "startBpm", Bpm.encode e.startBpm )
        , ( "subdivision", E.int (perBeat e.subdivision) )
        , ( "video", E.string (Maybe.withDefault "" e.video) )
        ]


decoder : Decoder Exercise
decoder =
    D.map5 Exercise
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "startBpm" Bpm.decoder)
        (D.field "subdivision" subdivisionDecoder)
        (D.field "video" (D.map nonEmpty D.string))


nonEmpty : String -> Maybe String
nonEmpty s =
    if String.trim s == "" then
        Nothing

    else
        Just (String.trim s)



-- VIDEO


{-| The YouTube video id of a watch, short, shorts, live or embed link.
-}
youtubeId : String -> Maybe String
youtubeId link =
    Url.fromString (String.trim link)
        |> Maybe.andThen
            (\url ->
                let
                    path =
                        String.split "/" url.path |> List.filter ((/=) "")

                    host =
                        String.toLower url.host
                in
                if host == "youtu.be" then
                    List.head path

                else if host == "youtube.com" || String.endsWith ".youtube.com" host then
                    case path of
                        [ "watch" ] ->
                            url.query
                                |> Maybe.withDefault ""
                                |> String.split "&"
                                |> List.filterMap
                                    (\pair ->
                                        case String.split "=" pair of
                                            [ "v", v ] ->
                                                Just v

                                            _ ->
                                                Nothing
                                    )
                                |> List.head

                        [ kind, id ] ->
                            if List.member kind [ "shorts", "embed", "live" ] then
                                Just id

                            else
                                Nothing

                        _ ->
                            Nothing

                else
                    Nothing
            )
        |> Maybe.andThen
            (\id ->
                if id /= "" && String.all (\c -> Char.isAlphaNum c || c == '-' || c == '_') id then
                    Just id

                else
                    Nothing
            )
