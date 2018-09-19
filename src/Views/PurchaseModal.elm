module Views.PurchaseModal exposing (purchaseModal)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Models exposing (PendingEvent, ItemType)
import Msgs exposing (Msg)
import Utils.HtmlUtils exposing (onFocusOut)
import Utils.StringUtils exposing (maybeItemTypeToString)

--- PUBLIC ---

purchaseModal : PendingEvent -> Html Msg
purchaseModal event =
    div [ class "modal fade"
        , id "purchaseModal"
        , tabindex -1
        , attribute "role" "dialog"
        , attribute "aria-hidden" "true"
        , attribute "aria-labelledby" "#purchaseModalLabel" ]
        [ div [ class "modal-dialog modal-dialog-centered", attribute "role" "document" ]
            [ div 
                [ class "modal-content" ] 
                (modalContent event)
            ]
        ]

--- PRIVATE ---

modalContent : PendingEvent -> List (Html Msg)
modalContent event =
    [ modalHeader event.itemType
    , modalBody event
    , modalFooter event
    ]

modalHeader : Maybe ItemType -> Html Msg
modalHeader itemType =
    div [ class "modal-header" ]
        [ h5 [ class "modal-title", id "purchaseModalLabel" ]
            [ text ("Purchase " ++ (maybeItemTypeToString itemType)) ]
        , button 
            [ type_ "button"
            , class "close"
            , attribute "data-dismiss" "modal"
            , attribute "aria-label" "Close"
            ]
            [ span [ attribute "aria-hidden" "true" ] [ text "x" ] ]
        ]

modalBody : PendingEvent -> Html Msg
modalBody event = 
    let
        nameValueOrPlaceholder =
            case event.editableData.name of
                Just theName ->
                    value theName
                Nothing ->
                    placeholder "Generic Brand"
        quantityValueOrPlaceholder =
            case event.editableData.quantity of
                Just theQuantity ->
                    value (toString theQuantity)
                Nothing ->
                    placeholder "1.0"
    in
        div [ class "modal-body" ]
            [ Html.form [ class "text-left px-3" ]
                [ div [ class "form-group" ]
                    [ label [ for "itemName" ] [ text ((maybeItemTypeToString event.itemType) ++ " Name:") ]
                    , input 
                        [ type_ "text"
                        , class "form-control"
                        , nameValueOrPlaceholder
                        , id "itemName"
                        , onInput Msgs.UpdatePendingEventItemName
                        ] []
                    ]
                , div [ class "form-group" ]
                    [ label [ for "itemQuantity" ] [ text "Quantity:"]
                    , input
                        [ type_ "text"
                        , class "form-control"
                        , quantityValueOrPlaceholder
                        , id "itemQuantity"
                        , onFocusOut Msgs.UpdatePendingEventQuantity
                        ] []
                    ]
                , div [ class "form-check" ]
                    [ input
                        [ type_ "checkbox"
                        , class "form-check-input"
                        , id "saveAsDefault"
                        , onClick Msgs.UpdatePendingEventSaveAsDefault
                        ] []
                    , label [ for "saveAsDefault", class "form-check-label" ] 
                        [ text "Save as default?" ]
                    ]
                ]
            ]

modalFooter : PendingEvent -> Html Msg
modalFooter event =
    let
        saveBtnAttributes =
            case (event.itemType, event.editableData.name, event.editableData.quantity) of
                (Just itemType, Just name, Just quantity) -> 
                    [ type_ "button"
                    , class ("btn btn-primary")
                    , onClick (Msgs.SavePendingEvent itemType name quantity)
                    , attribute "data-dismiss" "modal"
                    ]
                _ -> 
                    [ type_ "button"
                    , class ("btn btn-primary disabled")
                    ]

    in
        div [ class "modal-footer" ]
            [ button 
                [ type_ "button"
                , class "btn btn-secondary"
                , onClick Msgs.DeletePendingEvent
                , attribute "data-dismiss" "modal"
                ]
                [ text "Close" ]
            , button 
                saveBtnAttributes
                [ text "Save" ]
            ]