module View exposing (view)

import DateUtils exposing (toIso8601String, toPrettyString)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, href, id, src, type_)
import Html.Events exposing (onClick)
import Models exposing (Model, Event, EventType, ItemType)
import Msgs exposing (Msg)

view : Model -> Html Msg
view model =
    div []
        [ settingsPane model.showSettings
        , settingsIcon
        , header
        , container (columns model.events)
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

settingsPane : Bool -> Html Msg
settingsPane visible =
    let
        displayClass =
            if visible then
                ""
            else
                "d-none"
    in
        
    div [ class ("shadow-lg position-absolute w-25 h-100 " ++ displayClass), id "settings-pane" ] 
        [ settingsHeader
        , settingsForm
        ]

settingsHeader : Html Msg
settingsHeader =
    h3 [ class "text-primary text-center my-4" ] [ text "Settings" ]

settingsForm : Html Msg
settingsForm =
    form [ class "text-left px-3" ]
        [ div [ class "form-group" ]
            [ label [ attribute "for" "dog-name" ] [ text "Dog Name:" ]
            , input 
                [ type_ "text"
                , class "form-control"
                , attribute "placeholder" "Fido"
                , id "dog-name"
                ] []
            ]
        , a 
            [ class "btn btn-danger ml-2 float-right text-white"
            , onClick Msgs.ToggleShowSettings
            ] 
            [ text "Cancel" ]
        , a 
            [ class "btn btn-success float-right text-white"
            , onClick Msgs.SaveSettings
            ] 
            [ text "Save" ]
        ]

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

columnHeader : String -> Html Msg
columnHeader title =
    h2 [ class "text-center" ]
        [ text title ]

blockButton : String -> Msg -> Html Msg
blockButton message clickMsg =
    button [ class "btn btn-primary btn-block", onClick clickMsg ] [ text message ]

purchasedButton : ItemType -> String -> Float -> Html Msg
purchasedButton itemType name quantity =
    blockButton "Purchased!" (Msgs.RequestPurchaseEvent itemType name quantity) 

administeredButton : ItemType -> String -> Html Msg
administeredButton itemType name =
    blockButton "Administered!" (Msgs.RequestAdministerEvent itemType name)

row :  List (Html Msg) -> Html Msg
row columns =
    div [ class "row" ] columns

buttonRow : ItemType -> String -> Html Msg
buttonRow itemType itemName =
    row 
    [ div [ class "col-sm" ] [ (purchasedButton itemType itemName 1.0) ]
    , div [ class "col-sm" ] [ (administeredButton itemType itemName) ]
    ]
