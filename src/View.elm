module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Models exposing (Model, Event, EventType, ItemType, RememberedPurchase, RememberedPurchases)
import Msgs exposing (Msg)
import Utils.DateUtils exposing (toIso8601String, toPrettyString)
import Utils.StringUtils exposing (..)
import Views.PurchaseModal exposing (purchaseModal)
import Views.Settings exposing (settingsPane)

view : Model -> Html Msg
view model =
    div []
        [ text model.error
        , settingsPane model.showSettings model.settings model.defaultPurchases
        , settingsIcon
        , header
        , container (columns model.events model.defaultPurchases model.lastPurchases)
        , purchaseModal model.pendingEvent
        ]

settingsIcon : Html Msg
settingsIcon =
    a 
        [ class "position-absolute ml-2"
        , onClick Msgs.ToggleShowSettings
        , id "settings-icon"
        ] 
        [ img [ src "./settings.png" ] [] ]

header : Html Msg
header =    
    h1 [ class "display-2 text-center" ] 
        [ img [ src "./pets.png" ] []
        , text "dog-do"
        , img [ src "./pets.png" ] []
        ]

container : Html Msg -> Html Msg
container body =
    div [ class "container" ]
        [ body ]

columns : List Event -> RememberedPurchases -> RememberedPurchases -> Html Msg
columns events defaults lasts = 
    row [ genericColumn Models.Food events defaults.food lasts.food
        , genericColumn Models.HeartwormMedicine events defaults.heartwormMedicine lasts.heartwormMedicine
        , genericColumn Models.FleaTickMedicine events defaults.fleaTickMedicine lasts.fleaTickMedicine
        ]

genericColumn : ItemType -> List Event -> RememberedPurchase -> RememberedPurchase -> Html Msg
genericColumn itemType allEvents default last =
    let
        header =
            headerTextFromItemType itemType
        name =
            nameFromDefaultAndLast default.name last.name
        quantity = 
            quantityFromDefaultAndLast default.quantity last.quantity
        events =
            List.filter (\e -> e.itemType == itemType) allEvents
        buttons =
            if itemType == Models.Food then
                purchasedButton itemType name quantity
            else
                buttonRow itemType name quantity
    in
        div [ class "col" ]
            [ columnHeader header
            , stock events
            , buttons
            , eventList events
            ]

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

stock : List Event -> Html Msg
stock events =
    let
        amountPurchased = 
            events
            |> List.filter (\e -> e.eventType == Models.PurchaseEvent)
            |> List.map (\e -> e.quantity)
            |> List.sum
        
        amountAdministered =
            events
            |> List.filter (\e -> e.eventType == Models.AdministerEvent)
            |> List.map (\e -> e.quantity)
            |> List.sum
        
        amountRemaining = amountPurchased - amountAdministered
    in
        p [] [ text ("Amount remaining: " ++ (toString amountRemaining)) ]

eventList : List Event -> Html Msg
eventList events =  
    table [ class "table mt-3" ]
        [ tbody [] (List.map eventItem events) ]

eventItem : Event -> Html Msg
eventItem event =
    let
        eventClass = 
            if event.eventType == Models.PurchaseEvent then
                "table-success"
            else
                "table-primary"
    in
        tr [ class (eventClass ++ " shadow-sm p-3 m-1 rounded") ] 
            [ td [ class "text-left" ] 
                [ text (eventText event) ]
            ]

eventText : Event -> String
eventText event =
    let
        quantityText = 
            if event.eventType == Models.PurchaseEvent then
                " " ++ (toString event.quantity)
            else
                ""
    in
        (toPrettyString event.timestamp)
        ++ ": "
        ++ (eventTypeToString event.eventType)
        ++ " "
        ++ quantityText
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
        blockButton "Purchased!" buttonAttributes (Msgs.NewPurchaseEventRequestTimestamp itemType name quantity) 

administeredButton : ItemType -> String -> Html Msg
administeredButton itemType name =
    blockButton "Administered!" [] (Msgs.NewAdministerEventRequestTimestamp itemType name)

row :  List (Html Msg) -> Html Msg
row columns =
    div [ class "row" ] columns

buttonRow : ItemType -> String -> Float -> Html Msg
buttonRow itemType itemName itemQuantity =
    row 
    [ div [ class "col-sm" ] [ (purchasedButton itemType itemName itemQuantity) ]
    , div [ class "col-sm" ] [ (administeredButton itemType itemName) ]
    ]

headerTextFromItemType : ItemType -> String
headerTextFromItemType itemType =
    case itemType of
        Models.Food ->
            "Food"
        Models.HeartwormMedicine ->
            "Heartworm Medicine"
        Models.FleaTickMedicine ->
            "Flea/Tick Medicine"