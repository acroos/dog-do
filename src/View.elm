module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Models exposing (Model)
import Msgs exposing (Msg)
import Views.EditEventModal exposing (editEventModal)
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
        , editEventModal model.eventToEdit
        ]

--- PRIVATE ---

settingsIcon : Html Msg
settingsIcon =
    button
        [ attribute "data-toggle" "modal" 
        , attribute "data-target" "#settingsModal"
        , class "btn btn-link position-absolute ml-2 mt-2"
        , id "settings-icon"
        ] 
        [ img [ src "./settings.png" ] [] ]

header : Html Msg
header =    
    h1 [ class "display-2 text-center" ] 
        [ img [ src "./pets.png", class "mr-3" ] []
        , text "dog-do"
        , img [ src "./pets.png", class "ml-3" ] []
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