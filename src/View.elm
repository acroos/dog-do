module View exposing (view)

import DateUtils exposing (toIso8601String, toPrettyString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onCheck, onClick, onInput, targetValue)
import Json.Decode
import Models exposing (Model, Event, EventType, ItemType, RememberedPurchases, RememberedPurchase, UnitSystem, Dog)
import Msgs exposing (Msg)

view : Model -> Html Msg
view model =
    div []
        [ text model.error
        , settingsPane model.showSettings model.dog model.unitSystem model.defaultPurchases
        , settingsIcon
        , header
        , container (columns model.events)
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

settingsPane : Bool -> Dog -> UnitSystem -> RememberedPurchases -> Html Msg
settingsPane visible dog unitSystem defaults =
    let
        displayClass =
            if visible then
                ""
            else
                "d-none"
    in
        
    div [ class ("shadow-lg position-absolute w-25 h-100 " ++ displayClass), id "settings-pane" ] 
        [ settingsForm dog unitSystem defaults ]

settingsForm : Dog -> UnitSystem -> RememberedPurchases -> Html Msg
settingsForm dog unitSystem defaults =
    Html.form [ class "text-left px-3" ]
        [ h2 [ class "text-primary text-center mt-4" ] [ text "Settings" ]
        , dogInput dog
        , unitSystemRadios unitSystem
        , h2 [ class "text-primary text-center" ] [ text "Defaults:" ]
        , settingsDefaultsFormGroup Models.Food defaults.food
        , settingsDefaultsFormGroup Models.HeartwormMedicine defaults.heartwormMedicine
        , settingsDefaultsFormGroup Models.FleaTickMedicine defaults.fleaTickMedicine
        , a 
            [ class "btn btn-success float-right text-white"
            , onClick Msgs.ToggleShowSettings
            ] 
            [ text "Done" ]
        ]

dogInput : Dog -> Html Msg
dogInput dog =
    let
        valueOrPlaceholder = 
            if dog.name == "" then
                (placeholder "Fido")
            else
                (value dog.name)
        attrs = 
            [ type_ "text"
            , class "form-control"
            , id "dogName"
            , valueOrPlaceholder
            ]
    in
        
    div [ class "form-group" ]
        [ label [ for "dogName" ] [ text "Dog Name:" ]
        , input attrs []
        ]

unitSystemRadios : UnitSystem -> Html Msg
unitSystemRadios unitSystem =
    let
        baseAttrs = 
            [ class "form-check-input"
            , type_ "radio"
            , name "unitSystemRadios"
            ]
        
        baseAttrsMetric = 
            baseAttrs
            |> List.append [ value "metric", onClick (Msgs.SettingsUpdateUnitSystem Models.Metric) ]

        baseAttrsImperial =
            baseAttrs
            |> List.append [ value "imperial", onClick (Msgs.SettingsUpdateUnitSystem Models.Imperial) ]
        
        attrsMetric =
            case unitSystem of
                Models.Metric ->
                    (attribute "checked" "") :: baseAttrs
                Models.Imperial ->
                    baseAttrs

        attrsImperial =
            case unitSystem of
                Models.Metric ->
                    baseAttrs
                Models.Imperial ->
                    (attribute "checked" "") :: baseAttrs
    in
        
    div [ class "form-group" ]
            [ label [] [ text "Unit System:"]
            , div [ class "form-check" ]
                [ input attrsMetric []
                , label [ class "form-check-label", for "radioMetric" ]
                    [ text "Metric (kgs)" ]
                ]
            , div [ class "form-check" ]
                [ input attrsImperial []
                , label [ class "form-check-label", for "radioImperial" ]
                    [ text "Imperial (lbs)" ]
                ]
            ]

settingsDefaultsFormGroup : ItemType -> Maybe RememberedPurchase -> Html Msg
settingsDefaultsFormGroup itemType maybeRememberedPurchase =
    let
        nameLabel =
            (itemTypeToString itemType) ++ " Name:"
        nameValueOrPlaceholder =
            case maybeRememberedPurchase of
                Just purchase ->
                    (value purchase.name)
                Nothing ->
                    (placeholder "The good stuff")
        
        quantityValueOrPlaceholder =
            case maybeRememberedPurchase of
                Just purchase ->
                    (value (toString purchase.quantity))
                Nothing ->
                    (placeholder "1")

        nameAttrs = 
            [ type_ "text"
            , class "form-control"
            , id "itemName"
            , onInput (Msgs.SettingsUpdateDefaultsName itemType) 
            , nameValueOrPlaceholder
            ]
        
        quantityAttrs =
            [ type_ "text"
            , class "form-control"
            , id "itemQuantity"
            , onFocusOut (Msgs.SettingsUpdateDefaultsQuantity itemType)
            , quantityValueOrPlaceholder
            ]
    in
        div [ ]
            [ div [ class "form-group" ]
                [ label [ for "itemName" ] [ text nameLabel ]
                , input nameAttrs []
                ]
            , div [ class "form-group" ]
                [ label [ for "itemQuantity" ] [ text "Quantity:"]
                , input quantityAttrs []
                ]
            ]

onFocusOut : (String -> msg) -> Attribute msg
onFocusOut tagger =
  on "focusout" (Json.Decode.map tagger targetValue)

columns : List Event -> Html Msg
columns events = 
    row [ foodColumn (List.filter (\e -> e.itemType == Models.Food) events)
        , heartwormMedicineColumn (List.filter (\e -> e.itemType == Models.HeartwormMedicine) events)
        , fleaTickMedicineColumn (List.filter (\e -> e.itemType == Models.FleaTickMedicine) events)
        ]

foodColumn : List Event -> Html Msg
foodColumn events =
    div [ class "col" ]
        [ columnHeader "Food"
        , stock events
        , (purchasedButton Models.Food "Orijen Six Fish" 1.0)
        , eventList events
        ]

heartwormMedicineColumn : List Event -> Html Msg
heartwormMedicineColumn events =
    div [ class "col" ]
        [ columnHeader "Heartworm Medicine"
        , stock events
        , (buttonRow Models.HeartwormMedicine "Generic Brand")
        , eventList events
        ]

fleaTickMedicineColumn : List Event -> Html Msg
fleaTickMedicineColumn events =
    div [ class "col" ]
        [ columnHeader "Flea/Tick Medicine"
        , stock events
        , (buttonRow Models.FleaTickMedicine "Generic Brand")
        , eventList events
        ]

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

eventTypeToString : EventType -> String
eventTypeToString eventType =
    if eventType == Models.PurchaseEvent then
        "Purchased"
    else
        "Adminstered"

itemTypeToString : ItemType -> String
itemTypeToString itemType =
    case itemType of
        Models.HeartwormMedicine ->
            "Heartworm Medicine"
        Models.FleaTickMedicine ->
            "Flea/Tick Medicine"
        Models.Food ->
            "Food"

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

buttonRow : ItemType -> String -> Html Msg
buttonRow itemType itemName =
    row 
    [ div [ class "col-sm" ] [ (purchasedButton itemType itemName 1.0) ]
    , div [ class "col-sm" ] [ (administeredButton itemType itemName) ]
    ]

purchaseModal : Maybe Event -> Html Msg
purchaseModal event =
    let
        content = 
            case event of
                Just theEvent ->
                    modalContent theEvent
                Nothing ->
                    [ div [] [] ]
    in
        div [ class "modal fade"
            , id "purchaseModal"
            , tabindex -1
            , attribute "role" "dialog"
            , attribute "aria-hidden" "true"
            , attribute "aria-labelledby" "#purchaseModalLabel" ]
            [ div [ class "modal-dialog modal-dialog-centered", attribute "role" "document" ]
                [ div 
                    [ class "modal-content" ] 
                    content
                ]
            ]

modalContent : Event -> List (Html Msg)
modalContent event =
        [ modalHeader event.itemType
        , modalBody event
        , div [ class "modal-footer" ]
            [ button 
                [ type_ "button"
                , class "btn btn-secondary"
                , onClick Msgs.DeletePendingEvent
                , attribute "data-dismiss" "modal"
                ]
                [ text "Close" ]
            , button 
                [ type_ "button"
                , class "btn btn-primary"
                , onClick Msgs.SavePendingEvent
                , attribute "data-dismiss" "modal"
                ]
                [ text "Save" ]
            ]
        ]

modalHeader : ItemType -> Html Msg
modalHeader itemType =
    div [ class "modal-header" ]
        [ h5 [ class "modal-title", id "purchaseModalLabel" ]
            [ text ("Purchase " ++ (itemTypeToString itemType)) ]
        , button 
            [ type_ "button"
            , class "close"
            , attribute "data-dismiss" "modal"
            , attribute "aria-label" "Close"
            ]
            [ span [ attribute "aria-hidden" "true" ] [ text "x" ] ]
        ]

modalBody : Event -> Html Msg
modalBody event = 
    div [ class "modal-body" ]
        [ Html.form [ class "text-left px-3" ]
            [ div [ class "form-group" ]
                [ label [ for "itemName" ] [ text ((itemTypeToString event.itemType) ++ " Name:") ]
                , input 
                    [ type_ "text"
                    , class "form-control"
                    , value event.itemName
                    , id "itemName"
                    , onInput Msgs.UpdatePendingEventItemName
                    ] []
                ]
            , div [ class "form-group" ]
                [ label [ for "itemQuantity" ] [ text "Quantity:"]
                , input
                    [ type_ "text"
                    , class "form-control"
                    , value (toString event.quantity)
                    , id "itemQuantity"
                    , onFocusOut Msgs.UpdatePendingEventQuantity
                    ] []
                ]
            ]
        ]
