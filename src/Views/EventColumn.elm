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
import Utils.HtmlUtils exposing (empty)
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

        mostRecentPurchaseEventForType =
            model.events
            |> List.filter (\e -> e.eventType == Models.PurchaseEvent)
            |> List.filter (\e -> e.itemType == itemType)
            |> lastEvent

        mostRecentName =
            mostRecentPurchaseEventForType
            |> andThen (\e -> Just e.itemName)
        
        mostRecentQuantity =
            mostRecentPurchaseEventForType
            |> andThen (\e -> Just e.quantity)

        name =
            nameFromDefaultAndLast default.name mostRecentName

        quantity = 
            quantityFromDefaultAndLast default.quantity mostRecentQuantity

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

nameFromDefaultAndLast : Maybe String -> Maybe String -> Maybe String
nameFromDefaultAndLast default last =
    valueFromDefaultLastAndFallback default last

quantityFromDefaultAndLast : Maybe Float -> Maybe Float -> Maybe Float
quantityFromDefaultAndLast default last =
    valueFromDefaultLastAndFallback default last

valueFromDefaultLastAndFallback : Maybe a -> Maybe a -> Maybe a
valueFromDefaultLastAndFallback default last =
    case default of
        Just val -> (Just val)
        Nothing -> last

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
        amountSummaryList
            [ amountRemainingListItem amountText
            , daysTilPurchaseListItem daysText
            ]

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
                    Maybe.map (\i -> i * amountRemaining) maybeInterval

        daysToAdminister =
            case lastAdministerEvent of
                Just theEvent ->
                    calculateDaysToNextAdminister today maybeInterval theEvent
                Nothing -> Nothing

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

        daysToAdministerText =
            case daysToAdminister of
                Just amount -> (toString amount)
                Nothing -> "n/a"
    in
        amountSummaryList
            [ amountRemainingListItem amountText
            , daysTilPurchaseListItem daysText
            , daysTilAdministerListItem daysToAdministerText
            ]

amountRemainingListItem : String -> Html Msg
amountRemainingListItem amountString =
    summaryListItem "Amount remaining" amountString

daysTilPurchaseListItem : String -> Html Msg
daysTilPurchaseListItem daysString =
    summaryListItem "Days until next purchase" daysString

daysTilAdministerListItem : String -> Html Msg
daysTilAdministerListItem daysString =
    summaryListItem "Days until next administer" daysString

summaryListItem : String -> String -> Html Msg
summaryListItem title value =
    li [ class "list-group-item" ]
        [ text (title ++ ": " ++ value) ]

amountSummaryList : List (Html Msg) -> Html Msg
amountSummaryList listItems =
    ul [ class "list-group list-group-flush text-center" ]
        listItems

lastEvent : List Event -> Maybe Event
lastEvent events =
    events
    |> List.sortWith sortEventsByDateDescending
    |> List.head

eventList : UnitSystem -> List Event -> Html Msg
eventList unitSystem events =
    if (List.length events) > 0 then
        table [ class "table mt-3" ]
            [ thead []
                [ th [ class "text-center" ] [ text "History" ] ]
            , tbody [] (List.map (eventItem unitSystem) events) 
            ]
    else
        empty

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

purchasedButton : ItemType -> Maybe String -> Maybe Float -> Html Msg
purchasedButton itemType name quantity =
    let
        buttonAttributes =
            [ attribute "data-toggle" "modal" 
            , attribute "data-target" "#purchaseModal"
            ]
    in
        blockButton "Purchased!" buttonAttributes (Msgs.NewPendingPurchaseEvent itemType name quantity) 

administeredButton : ItemType -> Maybe String -> Html Msg
administeredButton itemType maybeName =
    let
        name =
            case maybeName of
                Just theName -> theName
                Nothing -> "Unknown"
    in
        blockButton "Administered!" [] (Msgs.NewAdministerEvent itemType name)

buttonRow : ItemType -> Maybe String -> Maybe Float -> Html Msg
buttonRow itemType itemName itemQuantity =
    divRow 
        [ div [ class "col" ] [ (purchasedButton itemType itemName itemQuantity) ]
        , div [ class "col" ] [ (administeredButton itemType itemName) ]
        ]
