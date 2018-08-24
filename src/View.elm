module View exposing (view)

import DateUtils exposing (toIso8601String, toPrettyString)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Models exposing (Model, Event, EventType, ItemType)
import Msgs exposing (Msg)

view : Model -> Html Msg
view model =
    div []
        [ text model.error
        , header
        , container (columns model.events)
        ]

header : Html Msg
header =    
    h1 [ class "display-2" ] [ text "dog-do" ]

container : Html Msg -> Html Msg
container body =
    div [ class "container" ]
        [ body ]

columns : List Event -> Html Msg
columns events = 
    row [ foodColumn (List.filter (\e -> e.itemType == Models.Food) events)
        , heartwormMedicineColumn (List.filter (\e -> e.itemType == Models.HeartwormMedicine) events)
        , fleaTickMedicineColumn (List.filter (\e -> e.itemType == Models.FleaTickMedicine) events)
        ]

foodColumn : List Event -> Html Msg
foodColumn events =
    div [ class "col-sm" ]
        [ columnHeader "Food"
        , eventList events
        , (purchasedButton Models.Food "Orijen Six Fish" 1.0)
        ]

heartwormMedicineColumn : List Event -> Html Msg
heartwormMedicineColumn events =
    div [ class "col-sm" ]
        [ columnHeader "Heartworm Medicine"
        , eventList events
        , (buttonRow Models.HeartwormMedicine "Generic Brand")
        ]

fleaTickMedicineColumn : List Event -> Html Msg
fleaTickMedicineColumn events =
    div [ class "col-sm" ]
        [ columnHeader "Flea/Tick Medicine"
        , eventList events
        , (buttonRow Models.FleaTickMedicine "Generic Brand")
        ]

eventList : List Event -> Html Msg
eventList events =  
    table [ class "table table-striped" ]
        [ tbody [] (List.map eventItem events) ]

eventItem : Event -> Html Msg
eventItem event =
    tr [] [ td [] [ text (eventText event) ] ]

eventText : Event -> String
eventText event =
    let
        quantityText = 
            if event.quantity > 0 then
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
