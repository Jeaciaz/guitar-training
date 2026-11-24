module Format exposing (ago, clock, dayMs, dayStamp, duration, startOfDay, subdivision, weekday)

{-| Russian display text for times, dates and subdivisions.
-}

import Exercise exposing (Subdivision(..))
import Stats
import Time


dayMs : Int
dayMs =
    24 * 60 * 60 * 1000


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


startOfDay : Time.Zone -> Int -> Int
startOfDay zone millis =
    let
        t =
            Time.millisToPosix millis
    in
    millis
        - (Time.toHour zone t * 3600000 + Time.toMinute zone t * 60000 + Time.toSecond zone t * 1000 + Time.toMillis zone t)


{-| "2026-11-24", for file names.
-}
dayStamp : Time.Zone -> Int -> String
dayStamp zone millis =
    let
        key =
            Stats.dayKey zone millis
    in
    String.fromInt (key // 10000) ++ "-" ++ pad2 (modBy 100 (key // 100)) ++ "-" ++ pad2 (modBy 100 key)


weekday : Time.Zone -> Int -> String
weekday zone millis =
    case Time.toWeekday zone (Time.millisToPosix millis) of
        Time.Mon ->
            "пн"

        Time.Tue ->
            "вт"

        Time.Wed ->
            "ср"

        Time.Thu ->
            "чт"

        Time.Fri ->
            "пт"

        Time.Sat ->
            "сб"

        Time.Sun ->
            "вс"


{-| "сегодня", "вчера", "5 дн назад".
-}
ago : Time.Zone -> Int -> Int -> String
ago zone now millis =
    let
        days =
            (startOfDay zone now - startOfDay zone millis) // dayMs
    in
    if days <= 0 then
        "сегодня"

    else if days == 1 then
        "вчера"

    else
        String.fromInt days ++ " дн назад"


subdivision : Subdivision -> String
subdivision sub =
    case sub of
        Quarter ->
            "♩ четверти"

        Eighth ->
            "♫ восьмые"

        Triplet ->
            "3 триоли"

        Sixteenth ->
            "♬ шестнадцатые"
