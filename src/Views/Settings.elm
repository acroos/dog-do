module Views.Settings exposing (settingsPane)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Models exposing (ItemType, RememberedPurchases, RememberedPurchase, UnitSystem, Dog)
import Msgs exposing (Msg)
import Utils.HtmlUtils exposing (onFocusOut)
import Utils.StringUtils exposing (itemTypeToString)

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
