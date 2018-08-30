module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Models exposing (Model)
import Msgs exposing (Msg)
import Utils.DateUtils exposing (toIso8601String, toPrettyString)
import Utils.StringUtils exposing (..)
import Views.EventColumn exposing (column)
import Views.PurchaseModal exposing (purchaseModal)
import Views.Settings exposing (settingsPane)
import Views.Shared exposing (divCol, divRow)

--- PUBLIC ---

view : Model -> Html Msg
view model =
    div []
        [ text model.error
        , settingsPane model.settings model.defaultPurchases
        , settingsIcon
        , header
        , container (columns model)
        , purchaseModal model.pendingEvent
        ]

--- PRIVATE ---

settingsIcon : Html Msg
settingsIcon =
    button
        [ attribute "data-toggle" "modal" 
        , attribute "data-target" "#settingsModal"
        , class "btn btn-link position-absolute ml-2"
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

columns : Model -> Html Msg
columns model = 
    divRow 
        [ column Models.Food model
        , column Models.HeartwormMedicine model
        , column Models.FleaTickMedicine model
        ]