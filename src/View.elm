module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Models exposing (Model, PurchaseEvent, AdministerEvent)
import Msgs exposing (Msg)

view : Model -> Html Msg
view model =
    div []
        [ header
        , container columns
        , container (events model)
        ]

header : Html Msg
header =
    h1 [ class "display-2" ] [ text "dog-do" ]

container : Html Msg -> Html Msg
container body =
    div [ class "container" ]
        [ body ]

columns : Html Msg
columns = 
    row [ foodColumn
        , heartwormMedicineColumn
        , fleaTickMedicineColumn
        ]

events : Model -> Html Msg
events model =
    div []
    [ ul []
        (List.map purchaseEventItem model.purchaseEvents)
    , ul []
        (List.map administerEventItem model.administerEvents)
    ]

purchaseEventItem : PurchaseEvent -> Html Msg
purchaseEventItem event =
    li [] [ text ("[" ++ (toString event.date) ++ "] Purchased: " ++ event.name ++ " (" ++(toString event.quantity) ++ ")") ]

administerEventItem : AdministerEvent -> Html Msg
administerEventItem event =
    li [] [ text ("[" ++ (toString event.date) ++ "] Administered: " ++ event.name) ]

foodColumn : Html Msg
foodColumn =
    div [ class "col-sm" ]
        [ columnHeader "Food"
        , (purchasedButton "Food" 1.0)
        ]

heartwormMedicineColumn : Html Msg
heartwormMedicineColumn =
    div [ class "col-sm" ]
        [ columnHeader "Heartworm Medicine"
        , (buttonRow "HeartwormMedicine")
        ]

fleaTickMedicineColumn : Html Msg
fleaTickMedicineColumn =
    div [ class "col-sm" ]
        [ columnHeader "Flea/Tick Medicine"
        , (buttonRow "FleaTickMedicine")
        ]

columnHeader : String -> Html Msg
columnHeader title =
    h2 [ class "text-center" ]
        [ text title ]

blockButton : String -> Msg -> Html Msg
blockButton message clickMsg =
    button [ class "btn btn-primary btn-block", onClick clickMsg ] [ text message ]

purchasedButton : String -> Float -> Html Msg
purchasedButton name quantity =
    blockButton "Purchased!" (Msgs.RequestPurchaseEvent name quantity) 

administeredButton : String -> Html Msg
administeredButton name =
    blockButton "Administered!" (Msgs.RequestAdministerEvent name)

row :  List (Html Msg) -> Html Msg
row columns =
    div [ class "row" ] columns

buttonRow : String -> Html Msg
buttonRow rowType =
    row 
    [ div [ class "col-sm" ] [ (purchasedButton rowType 1.0) ]
    , div [ class "col-sm" ] [ (administeredButton rowType) ]
    ]
