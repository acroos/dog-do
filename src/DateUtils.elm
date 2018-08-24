module DateUtils exposing (jsonEncodeDate, toIso8601String)

import Date exposing (Date)
import Json.Encode exposing (Value, string)

jsonEncodeDate : Date -> Value
jsonEncodeDate date =
    string (toIso8601String date)

toIso8601String : Date -> String
toIso8601String date =
    (toString (Date.year date)) ++ "-" ++ (toString (monthInt date)) ++ "-" ++ (toString (Date.day date))

monthInt : Date -> Int
monthInt date =
    let
        month = Date.month date
    in
        case month of
            Date.Jan ->
                1
            Date.Feb ->
                2
            Date.Mar ->
                3
            Date.Apr ->
                4
            Date.May ->
                5
            Date.Jun ->
                6
            Date.Jul ->
                7
            Date.Aug ->
                8
            Date.Sep ->
                9
            Date.Oct ->
                10
            Date.Nov ->
                11
            Date.Dec ->
                12