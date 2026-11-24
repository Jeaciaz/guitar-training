module Bpm exposing (Bpm, add, decoder, encode, fromInt, max, toInt)

{-| A tempo, always within 20–400 so the metronome can play it.
-}

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type Bpm
    = Bpm Int


fromInt : Int -> Bpm
fromInt =
    Bpm << clamp 20 400


toInt : Bpm -> Int
toInt (Bpm n) =
    n


add : Int -> Bpm -> Bpm
add delta (Bpm n) =
    fromInt (n + delta)


max : Bpm -> Bpm -> Bpm
max (Bpm a) (Bpm b) =
    Bpm (Basics.max a b)


{-| Out-of-range values are clamped rather than rejected.
-}
decoder : Decoder Bpm
decoder =
    D.map fromInt D.int


encode : Bpm -> E.Value
encode =
    E.int << toInt
