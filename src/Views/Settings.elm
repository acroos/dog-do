module Views.Settings exposing (settingsPane)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Models exposing (ItemType, Defaults, EditableEventData, UnitSystem, Settings)
import Msgs exposing (Msg)
import Utils.HtmlUtils exposing (onChange, onFocusOut)
import Utils.StringUtils exposing (itemTypeToString, unitSystemToMassString)

--- PUBLIC ---

settingsPane : Settings -> Defaults -> Html Msg
settingsPane settings defaults =
    let
        modalBody =
            accordion settings defaults
    in
        div [ class "modal fade"
            , id "settingsModal"
            , tabindex -1
            , attribute "role" "dialog"
            , attribute "aria-hidden" "true"
            , attribute "aria-labelledby" "#settingsModalLabel" ]
            [ div [ class "modal-dialog modal-dialog-centered", attribute "role" "document" ]
                [ div 
                    [ class "modal-content" ] 
                    [ modalHeader, modalBody ]
                ]
            ]

--- PRIVATE ---

modalHeader : Html Msg
modalHeader =
    div [ class "modal-header" ]
        [ h5 [ class "modal-title", id "settingsModalLabel" ]
            [ text "Settings/Defaults" ]
        , button 
            [ type_ "button"
            , class "close"
            , attribute "data-dismiss" "modal"
            , attribute "aria-label" "Close"
            ]
            [ span [ attribute "aria-hidden" "true" ] [ text "x" ] ]
        ]

accordion : Settings -> Defaults -> Html Msg
accordion settings defaults =
    div [ class "accordion", id "accordion" ]
        [ settingsCard settings
        , defaultsCard settings.unitSystem defaults
        ]

settingsCard : Settings -> Html Msg
settingsCard settings =
    card "Settings" True 
        [ dogNameFormGroup settings.dogName
        , unitSystemRadios settings.unitSystem
        , dogFoodPerDayFormGroup settings.dogFoodPerDay settings.unitSystem
        , medicineIntervalFormGroup Models.HeartwormMedicine settings.heartwormMedicineInterval
        , medicineIntervalFormGroup Models.FleaTickMedicine settings.fleaTickMedicineInterval
        ]

defaultsCard : UnitSystem -> Defaults -> Html Msg
defaultsCard unitSystem defaults =
    card "Defaults" False 
        [ defaultsFormGroup Models.Food unitSystem defaults.food
        , defaultsFormGroup Models.HeartwormMedicine unitSystem defaults.heartwormMedicine
        , defaultsFormGroup Models.FleaTickMedicine unitSystem defaults.fleaTickMedicine
        ]

card : String -> Bool -> List (Html Msg) -> Html Msg
card kind show contents =
    let
        headerId = "heading" ++ kind
        divId = "collapse" ++ kind
        showClass =
            if show then "show"
            else ""
        
    in
        div [ class "card" ]
            [ div [ class "card-header", id headerId ]
                [ h5 [ class "mb-0" ]
                    [ button 
                        [ class "btn btn-link"
                        , type_ "button"
                        , attribute "data-toggle" "collapse"
                        , attribute "data-target" ("#" ++ divId)
                        , attribute "aria-expanded" (String.toLower (toString show))
                        , attribute "aria-controls" divId
                        ]
                        [ text kind ]
                    ]
                ]
            , div 
                [ class ("collapse " ++ showClass)
                , id divId
                , attribute "aria-labelledby" headerId
                , attribute "data-parent" "#accordion"
                ]
                [ div [ class "card-body" ]
                    contents
                ]
            ]

dogNameFormGroup : Maybe String -> Html Msg
dogNameFormGroup dogName =
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

dogFoodPerDayFormGroup : Maybe Float -> UnitSystem -> Html Msg
dogFoodPerDayFormGroup maybeFoodPerDay unitSystem =
    let
        inputId =
            "dogFoodPerDay"
        inputElement =
            floatInputGroup
                maybeFoodPerDay
                (unitSystemToMassString unitSystem)
                inputId
                Msgs.SettingsUpdateFoodPerDay
        labelText =
            "Dog Food Per Day:"
    in
        formGroup inputId labelText inputElement

medicineIntervalFormGroup : ItemType -> Maybe Int -> Html Msg
medicineIntervalFormGroup itemType maybeInterval =
    let
        inputId =
            (toString itemType) ++ "Interval"
        inputElement =
            intInputGroup
                maybeInterval
                "day(s)"
                inputId
                (Msgs.SettingsUpdateMedicineInterval itemType)
        labelText =
            (itemTypeToString itemType) ++ " Interval:"
    in
        formGroup inputId labelText inputElement

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

formGroup : String -> String -> Html Msg -> Html Msg
formGroup inputId labelText inputElement =
    div [ class "form-group" ]
        [ label [ for inputId ] [ text labelText ]
        , inputElement
        ]

floatInputGroup : Maybe Float -> String -> String -> (String -> Msg) -> Html Msg
floatInputGroup maybeValue unitName inputId focusOutMsg =
    let
        floatPlaceholder =
            "1.0"
        floatInputAttrs =
            [ type_ "number"
            , step "0.1"
            ]
    in
        inputGroup
            maybeValue
            floatInputAttrs
            floatPlaceholder
            unitName
            inputId
            focusOutMsg

intInputGroup : Maybe Int -> String -> String -> (String -> Msg) -> Html Msg
intInputGroup maybeValue unitName inputId focusOutMsg =
    let
        intPlaceholder =
            "10"
        intInputAttrs =
            [ type_ "number"
            , step "1"
            ]
    in
        inputGroup
            maybeValue
            intInputAttrs
            intPlaceholder
            unitName
            inputId
            focusOutMsg

stringInputGroup : Maybe String -> String -> String -> String -> (String -> Msg) -> Html Msg
stringInputGroup maybeValue stringPlaceholder unitName inputId focusOutMsg =
    let
        stringInputAttrs =
            [ type_ "text" ]
    in
        inputGroup
            maybeValue
            stringInputAttrs
            stringPlaceholder
            unitName
            inputId
            focusOutMsg

inputGroup : Maybe a -> List (Attribute Msg) -> String -> String -> String -> (String -> Msg) -> Html Msg
inputGroup maybeValue specificAttrs inputPlaceholder unitName inputId focusOutMsg =
    let
        valueOrPlaceholder =
            case maybeValue of
                Just quantity ->
                    (value (toString quantity))
                Nothing ->
                    (placeholder inputPlaceholder)

        baseAttrs =
            [ class "form-control"
            , id inputId
            , onFocusOut focusOutMsg
            , valueOrPlaceholder
            ]
        
        inputAttrs = List.append baseAttrs specificAttrs
    in
        div [ class "input-group" ]
            [ input inputAttrs []
            , div [ class "input-group-append" ]
                [ div [ class "input-group-text" ] [ text unitName ] ]
            ]

defaultsFormGroup : ItemType -> UnitSystem -> EditableEventData -> Html Msg
defaultsFormGroup itemType unitSystem remembered =
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
                unitSystemToMassString unitSystem
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
            [ type_ "number"
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
