module Update exposing (update)

import Char
import Commands exposing (saveEvent)
import Date exposing (Date)
import Json.Decode as Decode
import Msgs exposing (Msg)
import Models exposing (..)
import Task exposing (Task)
import Utils.JsonUtils exposing (decodeEvent)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.RequestPurchaseEvent itemType name quantity ->
            ( model, Task.perform (Msgs.ReceivePurchaseEvent itemType name quantity) Date.now)

        Msgs.RequestAdministerEvent itemType name ->
            ( model, Task.perform (Msgs.ReceiveAdministerEvent itemType name) Date.now)

        Msgs.ReceivePurchaseEvent itemType name quantity date ->
            let
                purchaseEvent = (createPurchaseEvent itemType name quantity date)
            in
                ( { model | pendingEvent = Just purchaseEvent }, Cmd.none )

        Msgs.ReceiveAdministerEvent itemType name date ->
            let
                administerEvent = (createAdministerEvent itemType name date)
            in
                ( model, (saveEvent administerEvent) )

        Msgs.DeletePendingEvent ->
            ( { model | pendingEvent = Nothing }, Cmd.none)

        Msgs.UpdatePendingEventItemName name ->
            let
                newPendingEvent = 
                    case model.pendingEvent of
                        Just theEvent ->
                            Just (updateEventName theEvent name)
                        Nothing ->
                            Nothing
            in                
                ( { model | pendingEvent = newPendingEvent }, Cmd.none )

        Msgs.UpdatePendingEventQuantity quantityString ->
            let
                newQuantity = 
                    quantityFloatFromStringWithEventDefault quantityString model.pendingEvent
                newPendingEvent = 
                    case model.pendingEvent of
                        Just theEvent ->
                            Just (updateEventQuantity theEvent newQuantity)
                        Nothing ->
                            Nothing
            in                
                ( { model | pendingEvent = newPendingEvent }, Cmd.none )

        Msgs.SavePendingEvent ->
            let
                event = model.pendingEvent
                lastPurchases = 
                    case event of 
                        Just theEvent ->
                            updateLastPurchase model.lastPurchases theEvent.itemType { name = theEvent.itemName, quantity = theEvent.quantity }
                        Nothing ->
                            model.lastPurchases
            in
                case event of
                    Just theEvent ->
                        ( { model | pendingEvent = Nothing, lastPurchases = lastPurchases }, saveEvent theEvent )
                    Nothing ->
                        ( model, Cmd.none )

        Msgs.GotEventFromDatabase event ->
            case Decode.decodeValue decodeEvent event of
                Ok val ->
                    ( { model | events = (val :: model.events) }, Cmd.none )
                Err err ->
                    ( { model | error = err }, Cmd.none )

        Msgs.ToggleShowSettings ->
            ( { model | showSettings = not model.showSettings }, Cmd.none )

        Msgs.SettingsUpdateUnitSystem unitSystem ->
            ( { model | unitSystem = unitSystem }, Cmd.none )

        Msgs.SettingsUpdateDefaultsName itemType name ->
            let
                maybeDefault =
                    fetchDefaultPurchase model itemType
                
                updatedDefault = 
                    case maybeDefault of
                        Just purchase ->
                            { purchase | name = name }
                        Nothing ->
                            { name = name, quantity = 1.0 }

                oldDefaults = 
                    model.defaultPurchases

                newDefaults =
                    updateDefaults oldDefaults itemType updatedDefault
            in
                ( { model | defaultPurchases = newDefaults }, Cmd.none )

        Msgs.SettingsUpdateDefaultsQuantity itemType quantityString ->
            let
                maybeDefault =
                    fetchDefaultPurchase model itemType
                
                strippedString =
                    String.filter isDigitOrDecimal quantityString

                newQuantity =
                    stringToFloatWithDefault quantityString 1.0

                updatedDefault = 
                    case maybeDefault of
                        Just purchase ->
                            { purchase | quantity = newQuantity }
                        Nothing ->
                            { name = "", quantity = newQuantity }

                oldDefaults = 
                    model.defaultPurchases

                newDefaults =
                    updateDefaults oldDefaults itemType updatedDefault
            in
                ( { model | defaultPurchases = newDefaults }, Cmd.none )

updateDefaults : RememberedPurchases -> ItemType -> RememberedPurchase -> RememberedPurchases
updateDefaults oldDefaults itemType newDefault =
    case itemType of
        Models.Food ->
            { oldDefaults | food = Just newDefault }
        Models.HeartwormMedicine ->
            { oldDefaults | heartwormMedicine = Just newDefault }
        Models.FleaTickMedicine ->
            { oldDefaults | fleaTickMedicine = Just newDefault }

updateLastPurchase : RememberedPurchases -> ItemType -> RememberedPurchase -> RememberedPurchases
updateLastPurchase oldLastPurchases itemType lastPurchase =
    case itemType of
        Models.Food ->
            { oldLastPurchases | food = Just lastPurchase }
        Models.HeartwormMedicine ->
            { oldLastPurchases | heartwormMedicine = Just lastPurchase }
        Models.FleaTickMedicine ->
            { oldLastPurchases | fleaTickMedicine = Just lastPurchase }

fetchDefaultPurchase : Model -> ItemType -> Maybe RememberedPurchase
fetchDefaultPurchase model itemType =
    case itemType of
        Models.Food ->
            model.defaultPurchases.food
        Models.HeartwormMedicine ->
            model.defaultPurchases.heartwormMedicine
        Models.FleaTickMedicine ->
            model.defaultPurchases.fleaTickMedicine

quantityFloatFromStringWithEventDefault : String -> Maybe Event -> Float
quantityFloatFromStringWithEventDefault quantityString maybeEvent =
    let
        defaultValue =
            case maybeEvent of
                Just theEvent ->
                    theEvent.quantity
                Nothing ->
                    1.0
    in
        stringToFloatWithDefault quantityString defaultValue

stringToFloatWithDefault : String -> Float -> Float
stringToFloatWithDefault string default =
    string
    |> String.filter isDigitOrDecimal
    |> String.toFloat
    |> Result.withDefault default

isDigitOrDecimal : Char -> Bool
isDigitOrDecimal char =
    (Char.isDigit char) || char == '.'