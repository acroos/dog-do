module Utils.ListUtils exposing (sortEventsByDate, sortEventsByDateDescending)

import Date exposing (toTime)
import Models exposing (Event)

--- PUBLIC ---

sortEventsByDate : Event -> Event -> Order
sortEventsByDate a b =
    compare (toTime a.timestamp) (toTime b.timestamp)

sortEventsByDateDescending : Event -> Event -> Order
sortEventsByDateDescending a b =
    reverseOrder (sortEventsByDate a b)

--- PRIVATE ---

reverseOrder : Order -> Order
reverseOrder order =
    case order of
        LT -> GT
        EQ -> EQ
        GT -> LT