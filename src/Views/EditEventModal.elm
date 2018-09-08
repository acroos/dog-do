module Views.EditEventModal exposing (editEventModal)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Models exposing (Event, ItemType)
import Msgs exposing (Msg)
import Utils.DateUtils exposing (toMMDDYYYY)
import Utils.HtmlUtils exposing (empty, onChange, onFocusOut)

--- PUBLIC ---

editEventModal : Maybe Event -> Html Msg
editEventModal maybeEvent =
    div [ class "modal fade"
        , id "editEventModal"
        , tabindex -1
        , attribute "role" "dialog"
        , attribute "aria-hidden" "true"
        , attribute "aria-labelledby" "#editEventModalLabel" ]
        [ div [ class "modal-dialog modal-dialog-centered", attribute "role" "document" ]
            [ div 
                [ class "modal-content" ] 
                (modalContent maybeEvent)
            ]
        ]

--- PRIVATE ---

modalContent : Maybe Event -> List (Html Msg)
modalContent maybeEvent =
    case maybeEvent of
        Just theEvent ->
            [ modalHeader theEvent.itemType
            , modalBody theEvent
            , modalFooter theEvent
            ]
        Nothing -> []

modalHeader : ItemType -> Html Msg
modalHeader itemType =
    div [ class "modal-header" ]
        [ h5 [ class "modal-title", id "editEventModalLabel" ]
            [ text "Edit" ]
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
        [ (nameInput event)
        , (quantityInput event)
        , (dateInput event)
        ]

modalFooter : Event -> Html Msg
modalFooter event =
    div [ class "modal-footer" ]
        [ button 
            [ type_ "button"
            , class "btn btn-secondary"
            , attribute "data-dismiss" "modal"
            ]
            [ text "Cancel" ]
        , button 
            [ type_ "button"
            , class ("btn btn-primary")
            , attribute "data-dismiss" "modal"
            , onClick Msgs.SaveEditedEvent
            ]
            [ text "Save" ]
        ]

nameInput : Event -> Html Msg
nameInput event =
    div [ class "form-group" ]
        [ label [ for "eventItemName" ] [ text "Kind:" ]
        , input
            [ type_ "text"
            , class "form-control"
            , value event.itemName
            , onFocusOut Msgs.UpdateEditedEventName
            ]
            []
        ]

quantityInput : Event -> Html Msg
quantityInput event =
    case event.eventType of
        Models.AdministerEvent ->
            empty
        Models.PurchaseEvent ->
            div [ class "form-group" ]
                [ label [ for "eventQuantity" ] [ text "Quantity:" ]
                , input
                    [ type_ "text"
                    , class "form-control"
                    , value (toString event.quantity)
                    , onFocusOut Msgs.UpdateEditedEventQuantity
                    ]
                    []
                ]

dateInput : Event -> Html Msg
dateInput event =
    div [ class "form-group" ]
        [ label [ for "eventDate" ] [ text "Date:" ]
        , input 
            [ type_ "date"
            , class "form-control"
            , value (toMMDDYYYY event.timestamp)
            , onFocusOut Msgs.UpdateEditedEventTimestamp
            ]
            []
        ]