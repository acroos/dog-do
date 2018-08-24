module DateUtils exposing (toIso8601String, toPrettyString)

import Date exposing (Date)

toIso8601String : Date -> String
toIso8601String date =
    let
        dateArray = 
            [ toString <| Date.year <| date
            , "-"
            , zeroPadInt2 <| extractMonthNum <| date
            , "-"
            , zeroPadInt2 <| Date.day <| date
            , "T"
            , zeroPadInt2 <| Date.hour <| date
            , ":"
            , zeroPadInt2 <| Date.minute <| date
            , ":"
            , zeroPadInt2 <| Date.second <| date
            , "."
            , zeroPadInt3 <| Date.millisecond <| date
            ]
    in
        String.join "" dateArray

toPrettyString : Date -> String
toPrettyString date =
    (toString (Date.day date)) 
    ++ " "
    ++ (toString (Date.month date))
    ++ " "
    ++ (toString (Date.year date))

zeroPadInt2 : Int -> String
zeroPadInt2 num =
    if num == 0 then
        "00"
    else if num < 10 then
        "0" ++ (toString num)
    else
        toString num

zeroPadInt3 : Int -> String
zeroPadInt3 num =
    if num < 10 then
        "0" ++ (zeroPadInt2 num)
    else if num < 100 then
        "0" ++ (toString num)
    else
        toString num

extractDayString : Date -> String
extractDayString date =
    let
        dayNumber = Date.day date
    in
        if dayNumber < 10 then
            "0" ++ (toString dayNumber)
        else
            (toString dayNumber)

extractMonthNum : Date -> Int
extractMonthNum date =
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