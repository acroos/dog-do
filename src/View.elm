module View exposing (view)

import DateUtils exposing (toIso8601String, toPrettyString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Models exposing (Model, Event, EventType, ItemType, RememberedPurchase, RememberedPurchases)
import Msgs exposing (Msg)
import Utils.StringUtils exposing (..)
import Views.PurchaseModal exposing (purchaseModal)
import Views.Settings exposing (settingsPane)

view : Model -> Html Msg
view model =
    div []
        [ text model.error
        , settingsPane model.showSettings model.dog model.unitSystem model.defaultPurchases
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
    row [ foodColumn (List.filter (\e -> e.itemType == Models.Food) events) defaults.food lasts.food
        , heartwormMedicineColumn (List.filter (\e -> e.itemType == Models.HeartwormMedicine) events) defaults.heartwormMedicine lasts.heartwormMedicine
        , fleaTickMedicineColumn (List.filter (\e -> e.itemType == Models.FleaTickMedicine) events) defaults.fleaTickMedicine lasts.fleaTickMedicine
        ]

foodColumn : List Event -> Maybe RememberedPurchase -> Maybe RememberedPurchase -> Html Msg
foodColumn events maybeDefault maybeLast =
    let
        name =
            nameFromDefaultLastAndFallback maybeDefault maybeLast "Generic Brand"
        quantity = 
            quantityFromDefaultLastAndFallback maybeDefault maybeLast 1.0
    in
        div [ class "col" ]
            [ columnHeader "Food"
            , stock events
            , (purchasedButton Models.Food name quantity)
            , eventList events
            ]

heartwormMedicineColumn : List Event ->Maybe RememberedPurchase -> Maybe RememberedPurchase ->  Html Msg
heartwormMedicineColumn events maybeDefault maybeLast =
    let
        name =
            nameFromDefaultLastAndFallback maybeDefault maybeLast "Generic Brand"
        quantity = 
            quantityFromDefaultLastAndFallback maybeDefault maybeLast 1.0
    in
        div [ class "col" ]
            [ columnHeader "Heartworm Medicine"
            , stock events
            , (buttonRow Models.HeartwormMedicine name quantity)
            , eventList events
            ]

fleaTickMedicineColumn : List Event ->Maybe RememberedPurchase -> Maybe RememberedPurchase ->  Html Msg
fleaTickMedicineColumn events maybeDefault maybeLast =
    let
        name =
            nameFromDefaultLastAndFallback maybeDefault maybeLast "Generic Brand"
        quantity = 
            quantityFromDefaultLastAndFallback maybeDefault maybeLast 1.0
    in
        div [ class "col" ]
            [ columnHeader "Flea/Tick Medicine"
            , stock events
            , (buttonRow Models.FleaTickMedicine name quantity)
            , eventList events
            ]


nameFromDefaultLastAndFallback : Maybe RememberedPurchase -> Maybe RememberedPurchase -> String -> String
nameFromDefaultLastAndFallback default last fallback =
    case default of
        Just purchase ->
            purchase.name
        Nothing ->
            case last of
                Just purchase ->
                    purchase.name
                Nothing ->
                    fallback

quantityFromDefaultLastAndFallback : Maybe RememberedPurchase -> Maybe RememberedPurchase -> Float -> Float
quantityFromDefaultLastAndFallback default last fallback =
    case default of
        Just purchase ->
            purchase.quantity
        Nothing ->
            case last of
                Just purchase ->
                    purchase.quantity
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
        blockButton "Purchased!" buttonAttributes (Msgs.RequestPurchaseEvent itemType name quantity) 

administeredButton : ItemType -> String -> Html Msg
administeredButton itemType name =
    blockButton "Administered!" [] (Msgs.RequestAdministerEvent itemType name)

row :  List (Html Msg) -> Html Msg
row columns =
    div [ class "row" ] columns

buttonRow : ItemType -> String -> Float -> Html Msg
buttonRow itemType itemName itemQuantity =
    row 
    [ div [ class "col-sm" ] [ (purchasedButton itemType itemName itemQuantity) ]
    , div [ class "col-sm" ] [ (administeredButton itemType itemName) ]
    ]