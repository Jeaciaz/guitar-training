module Stats exposing (Totals, dayKey, exerciseTotals, lastPlayed, latestTopBpm, msByDay)

{-| Aggregates over practice segments.
-}

import Bpm exposing (Bpm)
import Dict exposing (Dict)
import Session exposing (Segment)
import Time


{-| A local calendar day as yyyymmdd.
-}
dayKey : Time.Zone -> Int -> Int
dayKey zone millis =
    let
        t =
            Time.millisToPosix millis
    in
    Time.toYear zone t * 10000 + monthNumber (Time.toMonth zone t) * 100 + Time.toDay zone t


monthNumber : Time.Month -> Int
monthNumber m =
    case m of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


{-| Practice millis per local day (keyed by `dayKey`), attributed to the day
each segment started.
-}
msByDay : Time.Zone -> List Segment -> Dict Int Int
msByDay zone =
    List.foldl
        (\seg ->
            Dict.update (dayKey zone seg.startedAt)
                (\v -> Just (Maybe.withDefault 0 v + seg.durationMs))
        )
        Dict.empty


type alias Totals =
    { free : Int
    , byExercise : Dict String Int
    }


exerciseTotals : List Segment -> Totals
exerciseTotals =
    List.foldl
        (\seg acc ->
            case seg.exerciseId of
                Just id ->
                    { acc | byExercise = Dict.update id (\v -> Just (Maybe.withDefault 0 v + seg.durationMs)) acc.byExercise }

                Nothing ->
                    { acc | free = acc.free + seg.durationMs }
        )
        { free = 0, byExercise = Dict.empty }


{-| When each exercise was last played.
-}
lastPlayed : List Segment -> Dict String Int
lastPlayed =
    List.foldl
        (\seg acc ->
            case seg.exerciseId of
                Just id ->
                    Dict.update id (\v -> Just (max seg.startedAt (Maybe.withDefault 0 v))) acc

                Nothing ->
                    acc
        )
        Dict.empty


{-| The top tempo of the most recent segment that recorded one.
-}
latestTopBpm : String -> List Segment -> Maybe Bpm
latestTopBpm exerciseId segments =
    segments
        |> List.filter (\s -> s.exerciseId == Just exerciseId)
        |> List.filterMap (\s -> Maybe.map (Tuple.pair s.startedAt) s.topBpm)
        |> List.sortBy (Tuple.first >> negate)
        |> List.head
        |> Maybe.map Tuple.second
