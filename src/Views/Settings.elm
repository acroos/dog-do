module Views.Settings exposing (settingsPane)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Models exposing (ItemType, RememberedPurchases, RememberedPurchase, UnitSystem, Dog, Settings)
import Msgs exposing (Msg)
import Utils.HtmlUtils exposing (onChange, onFocusOut)
import Utils.StringUtils exposing (itemTypeToString)

settingsPane : Bool -> Settings -> RememberedPurchases -> Html Msg
settingsPane visible settings defaults =
    let
        displayClass =
            if visible then
                ""
            else
                "d-none"
    in
        
    div [ class ("shadow-lg position-absolute w-25 h-100 " ++ displayClass), id "settings-pane" ] 
        [ settingsForm settings defaults ]

settingsForm : Settings -> RememberedPurchases -> Html Msg
settingsForm settings defaults =
    Html.form [ class "text-left px-3" ]
        [ h2 [ class "text-primary text-center mt-4" ] [ text "Settings" ]
        , dogInput settings.dogName
        , unitSystemRadios settings.unitSystem
        , h2 [ class "text-primary text-center" ] [ text "Defaults:" ]
        , settingsDefaultsFormGroup Models.Food settings.unitSystem defaults.food
        , settingsDefaultsFormGroup Models.HeartwormMedicine settings.unitSystem defaults.heartwormMedicine
        , settingsDefaultsFormGroup Models.FleaTickMedicine settings.unitSystem defaults.fleaTickMedicine
        , a 
            [ class "btn btn-success float-right text-white"
            , onClick Msgs.ToggleShowSettings
            ] 
            [ text "Done" ]
        ]

dogInput : Maybe String -> Html Msg
dogInput dogName =
    let
        valueOrPlaceholder = 
            case dogName of
                Just name ->
                    value name
                Nothing ->
                    placeholder "Fido"
        attrs = 
            [ type_ "text"
            , class "form-control"
            , id "dogName"
            , valueOrPlaceholder
            , onFocusOut Msgs.SettingsUpdateDogName
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
                    (attribute "checked" "") :: baseAttrsMetric
                Models.Imperial ->
                    baseAttrsMetric

        attrsImperial =
            case unitSystem of
                Models.Metric ->
                    baseAttrsImperial
                Models.Imperial ->
                    (attribute "checked" "") :: baseAttrsImperial
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

settingsDefaultsFormGroup : ItemType -> UnitSystem -> RememberedPurchase -> Html Msg
settingsDefaultsFormGroup itemType unitSystem remembered =
    let
        nameLabel =
            (itemTypeToString itemType) ++ " Name:"

        nameValueOrPlaceholder =
            case remembered.name of
                Just name ->
                    (value name)
                Nothing ->
                    (placeholder "The good stuff")
        
        quantityValueOrPlaceholder =
            case remembered.quantity of
                Just quantity ->
                    (value (toString quantity))
                Nothing ->
                    (placeholder "1")
        
        quantityUnits = 
            if itemType == Models.Food then
                if unitSystem == Models.Metric then
                    "kg(s)"
                else
                    "lb(s)"
            else
                "dose(s)"

        nameAttrs = 
            [ type_ "text"
            , class "form-control"
            , id "itemName"
            , onFocusOut (Msgs.DefaultsUpdateNameForItemType itemType) 
            , nameValueOrPlaceholder
            ]
        
        quantityAttrs =
            [ type_ "text"
            , class "form-control"
            , id "itemQuantity"
            , onFocusOut (Msgs.DefaultsUpdateQuantityForItemType itemType)
            , quantityValueOrPlaceholder
            ]
        
        quantityInputGroup =
            div [ class "input-group" ]
                [ input quantityAttrs []
                , div [ class "input-group-append" ]
                    [ div [ class "input-group-text" ] [ text quantityUnits ] ]
                ]

        -- <div class="input-group mb-2">
        --  <div class="input-group-prepend">
        --       <div class="input-group-text">@</div>
        --  </div>
        --  <input type="text" class="form-control" id="inlineFormInputGroup" placeholder="Username">
    in
        div [ ]
            [ div [ class "form-group" ]
                [ label [ for "itemName" ] [ text nameLabel ]
                , input nameAttrs []
                ]
            , div [ class "form-group" ]
                [ label [ for "itemQuantity" ] [ text "Quantity:"]
                , quantityInputGroup
                ]
            ]
