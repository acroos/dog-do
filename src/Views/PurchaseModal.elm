module Views.PurchaseModal exposing (purchaseModal)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Models exposing (Event, ItemType)
import Msgs exposing (Msg)
import Utils.HtmlUtils exposing (onFocusOut)
import Utils.StringUtils exposing (itemTypeToString)

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
