module Utils.EventUtils exposing 
    ( calculateFoodRemaining
    , calculateFoodDaysRemaining
    , calculateMedicineRemaining
    , calculateMedicineDaysRemaining
    )

import Date exposing (Date)
import Maybe exposing (andThen)
import Models exposing (Event, EventType, PendingEvent)
import Utils.DateUtils exposing (daysBetween)

--- PUBLIC ---

calculateFoodRemaining : Date -> Maybe Float -> Event -> Maybe Float
calculateFoodRemaining today maybeFoodPerDay event =
    let
        daysSincePurchase =
            toFloat <| floor <| daysBetween event.timestamp today
    in
        case maybeFoodPerDay of
            Just amount ->
                Just (event.quantity - (amount * daysSincePurchase))
            Nothing ->
                Just event.quantity

calculateFoodDaysRemaining : Maybe Float -> Float -> Maybe Int
calculateFoodDaysRemaining maybeAmountPerDay amountRemaining =
    maybeAmountPerDay
    |> andThen (\perDay -> Just (floor (amountRemaining / perDay)))

calculateMedicineRemaining : List Event -> Float
calculateMedicineRemaining events =
    let
        purchased =
            amountForEventsByEventType Models.PurchaseEvent events
        administered =
            amountForEventsByEventType Models.AdministerEvent events
    in
        purchased - administered

calculateMedicineDaysRemaining : Int -> Date -> Maybe Int -> Event -> Maybe Int
calculateMedicineDaysRemaining amountRemaining today maybeInterval lastAdministerEvent =
    let
        daysSincePurchase =
            floor (daysBetween lastAdministerEvent.timestamp today)
    in
        maybeInterval
        |> andThen (\i -> Just (calculateMedicineDaysRemainingHelper amountRemaining i daysSincePurchase))

--- PRIVATE ---

amountForEventsByEventType : EventType -> List Event -> Float
amountForEventsByEventType eventType events =
    events
    |> List.filter (\e -> e.eventType == eventType)
    |> List.map (\e -> e.quantity)
    |> List.sum

calculateMedicineDaysRemainingHelper : Int -> Int -> Int -> Int
calculateMedicineDaysRemainingHelper amountRemaining interval daysSincePurchase =
    let
        futureIntervalDays =
            (amountRemaining - 1) * interval
        currentIntervalDays =
            interval - daysSincePurchase
    in
        currentIntervalDays + futureIntervalDays