module Theme exposing (Theme(..), all, decoder, encode, id, name)

{-| Colour themes. Each `id` matches a `[data-theme]` block in style.css.
-}

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type Theme
    = Sand
    | Paper
    | Mint
    | Lamp
    | Chocolate
    | Wine
    | Forest
    | Graphite
    | Night
    | Steel


{-| Light ones first, then warm dark, then cold dark.
-}
all : List Theme
all =
    [ Sand, Paper, Mint, Lamp, Chocolate, Wine, Forest, Graphite, Night, Steel ]


id : Theme -> String
id theme =
    case theme of
        Sand ->
            "sand"

        Paper ->
            "paper"

        Mint ->
            "mint"

        Lamp ->
            "lamp"

        Chocolate ->
            "chocolate"

        Wine ->
            "wine"

        Forest ->
            "forest"

        Graphite ->
            "graphite"

        Night ->
            "night"

        Steel ->
            "steel"


name : Theme -> String
name theme =
    case theme of
        Sand ->
            "Песок"

        Paper ->
            "Бумага"

        Mint ->
            "Мята"

        Lamp ->
            "Лампа"

        Chocolate ->
            "Шоколад"

        Wine ->
            "Вино"

        Forest ->
            "Хвоя"

        Graphite ->
            "Графит"

        Night ->
            "Ночь"

        Steel ->
            "Сталь"


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
