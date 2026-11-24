module Theme exposing (Theme(..), all, decoder, encode, id, name)

{-| Colour themes. Each `id` matches a `[data-theme]` block in style.css.
-}

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type Theme
    = Lamp
    | Forest
    | Sand
    | Graphite


all : List Theme
all =
    [ Lamp, Forest, Sand, Graphite ]


id : Theme -> String
id theme =
    case theme of
        Lamp ->
            "lamp"

        Forest ->
            "forest"

        Sand ->
            "sand"

        Graphite ->
            "graphite"


name : Theme -> String
name theme =
    case theme of
        Lamp ->
            "Лампа"

        Forest ->
            "Хвоя"

        Sand ->
            "Песок"

        Graphite ->
            "Графит"


{-| Unknown names fall back to the default theme.
-}
decoder : Decoder Theme
decoder =
    D.map
        (\s -> List.filter (\t -> id t == s) all |> List.head |> Maybe.withDefault Lamp)
        D.string


encode : Theme -> E.Value
encode =
    E.string << id
