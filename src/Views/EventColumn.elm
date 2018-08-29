module Views.EventColumn exposing (column)

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Maybe exposing (andThen)
import Models exposing (..)
import Msgs exposing (Msg)
import Utils.DateUtils exposing (daysBetween, timeBetween, toIso8601String, toPrettyString)
import Utils.EventUtils exposing (..)
import Utils.ListUtils exposing (sortEventsByDateDescending)
import Utils.StringUtils exposing (..)
import Views.Shared exposing (divCol, divRow)

--- PUBLIC ---
column : ItemType -> Model -> Html Msg
column itemType model =
    let
        header =
            itemTypeToString itemType

        default =
            case itemType of
                Models.Food ->
                    model.defaultPurchases.food
                Models.HeartwormMedicine ->
                    model.defaultPurchases.heartwormMedicine
                Models.FleaTickMedicine ->
                    model.defaultPurchases.fleaTickMedicine

        name =
            nameFromDefaultAndLast default.name Nothing

        quantity = 
            quantityFromDefaultAndLast default.quantity Nothing

        events =
            List.filter (\e -> e.itemType == itemType) model.events

        buttons =
            if itemType == Models.Food then
                purchasedButton itemType name quantity
            else
                buttonRow itemType name quantity
    in
        divCol 
            [ columnHeader header
            , quantityInfo events model.settings itemType model.now
            , buttons
            , eventList model.settings.unitSystem events
            ]

--- PRIVATE ---

nameFromDefaultAndLast : Maybe String -> Maybe String -> String
nameFromDefaultAndLast default last =
    valueFromDefaultLastAndFallback default last "Generic Brand"

quantityFromDefaultAndLast : Maybe Float -> Maybe Float -> Float
quantityFromDefaultAndLast default last =
    valueFromDefaultLastAndFallback default last 1.0

valueFromDefaultLastAndFallback : Maybe a -> Maybe a -> a -> a
valueFromDefaultLastAndFallback default last fallback =
    case default of
        Just val ->
            val
        Nothing ->
            case last of
                Just val ->
                    val
                Nothing ->
                    fallback

quantityInfo : List Event -> Settings -> ItemType -> Date -> Html Msg
quantityInfo events settings itemType today =
    case itemType of
        Models.Food ->
            foodAmountSummary (lastEvent events) settings.dogFoodPerDay settings.unitSystem today
        Models.HeartwormMedicine ->
            medicineAmountSummary events settings.heartwormMedicineInterval today
        Models.FleaTickMedicine ->
            medicineAmountSummary events settings.fleaTickMedicineInterval today

foodAmountSummary : Maybe Event -> Maybe Float -> UnitSystem -> Date -> Html Msg
foodAmountSummary maybeEvent maybeFoodPerDay unitSystem today =
    let
        amountRemaining =
            maybeEvent
            |> andThen (calculateFoodRemaining today maybeFoodPerDay)

        daysRemaining =
            amountRemaining
            |> andThen (calculateFoodDaysRemaining maybeFoodPerDay)

        amountText =
            case amountRemaining of
                Just amount -> (toString amount) ++ " " ++ (unitSystemToMassString unitSystem)
                Nothing -> "n/a"
        
        daysText =
            case daysRemaining of
                Just amount -> (toString amount)
                Nothing -> "n/a"
    in
        amountSummaryList amountText daysText

medicineAmountSummary : List Event -> Maybe Int -> Date -> Html Msg
medicineAmountSummary events maybeInterval today =
    let
        amountRemaining =
            floor (calculateMedicineRemaining events)

        lastAdministerEvent =
            events
            |> List.filter (\e -> e.eventType == Models.AdministerEvent)
            |> lastEvent

        daysRemaining =
            case lastAdministerEvent of
                Just theEvent ->
                    calculateMedicineDaysRemaining amountRemaining today maybeInterval theEvent
                Nothing ->
                    maybeInterval
                    |> andThen (\i -> Just (i * amountRemaining))

        amountText = 
            if amountRemaining < 0 then
                "n/a"
            else if amountRemaining == 1 then
                (toString amountRemaining) ++ " dose"
            else
                (toString amountRemaining) ++ " doses"

        daysText =
            case daysRemaining of
                Just amount -> (toString amount)
                Nothing -> "n/a"
    in
        amountSummaryList amountText daysText

amountSummaryList : String -> String -> Html Msg
amountSummaryList amountText daysText =
    ul [ class "list-group list-group-flush text-center" ]
        [ li [ class "list-group-item" ]
            [ text ("Amount remaining: " ++ amountText) ]
        , li [ class "list-group-item" ]
            [ text ("Days until next purchase: " ++ daysText) ]
        ]

lastEvent : List Event -> Maybe Event
lastEvent events =
    events
    |> List.sortWith sortEventsByDateDescending
    |> List.head

eventList : UnitSystem -> List Event -> Html Msg
eventList unitSystem events =
    table [ class "table mt-3" ]
        [ tbody [] (List.map (eventItem unitSystem) events) ]

eventItem :  UnitSystem -> Event -> Html Msg
eventItem unitSystem event =
    let
        eventClass = 
            if event.eventType == Models.PurchaseEvent then
                "table-success"
            else
                "table-primary"
    in
        tr [ class (eventClass ++ " shadow-sm p-3 m-1 rounded") ] 
            [ td [ class "text-left" ] 
                [ text (eventText unitSystem event) ]
            ]

eventText : UnitSystem -> Event -> String
eventText unitSystem event =
    let
        quantityText = 
            if event.eventType == Models.PurchaseEvent then
                " " ++ (toString event.quantity)
            else
                ""
        
        unitText =
            if event.eventType /= Models.PurchaseEvent then
                ""
            else if event.itemType == Models.Food then
                " " ++ (unitSystemToMassString unitSystem)
            else
                " dose(s)"

    in
        (toPrettyString event.timestamp)
        ++ ": "
        ++ (eventTypeToString event.eventType)
        ++ quantityText
        ++ unitText
        ++ " "
        ++ event.itemName

columnHeader : String -> Html Msg
columnHeader title =
    h2 [ class "text-center" ]
        [ text title ]

blockButton : String -> List (Attribute Msg) -> Msg -> Html Msg
blockButton message attributes clickMsg =
    let
        buttonAttributes =
            List.append attributes [ class "btn btn-primary btn-block", onClick clickMsg ]
    in
        button buttonAttributes [ text message ]

purchasedButton : ItemType -> String -> Float -> Html Msg
purchasedButton itemType name quantity =
    let
        buttonAttributes =
            [ attribute "data-toggle" "modal" 
            , attribute "data-target" "#purchaseModal"
            ]
    in
        blockButton "Purchased!" buttonAttributes (Msgs.NewPendingPurchaseEvent itemType name quantity) 

administeredButton : ItemType -> String -> Html Msg
administeredButton itemType name =
    blockButton "Administered!" [] (Msgs.NewAdministerEvent itemType name)

buttonRow : ItemType -> String -> Float -> Html Msg
buttonRow itemType itemName itemQuantity =
    divRow 
        [ div [ class "col" ] [ (purchasedButton itemType itemName itemQuantity) ]
        , div [ class "col" ] [ (administeredButton itemType itemName) ]
        ]
