module Update exposing (update)

import Char
import Commands exposing (..)
import Date exposing (Date)
import Json.Decode as Decode
import Msgs exposing (Msg)
import Models exposing (..)
import Task exposing (Task)
import Utils.JsonUtils exposing (decodeEvent, decodeRememberedPurchases, decodeSettings)

--- PUBLIC ---

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.NewPurchaseEventRequestTimestamp itemType name quantity ->
            ( model, Task.perform (Msgs.NewPurchaseEventReceiveTimestamp itemType name quantity) Date.now)

        Msgs.NewAdministerEventRequestTimestamp itemType name ->
            ( model, Task.perform (Msgs.NewAdministerEventReceiveTimestamp itemType name) Date.now)

        Msgs.NewPurchaseEventReceiveTimestamp itemType name quantity date ->
            let
                purchaseEvent = (createPurchaseEvent itemType name quantity date)
            in
                ( { model | pendingEvent = Just purchaseEvent }, Cmd.none )

        Msgs.NewAdministerEventReceiveTimestamp itemType name date ->
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
                            updateLastPurchase model.lastPurchases 
                                            theEvent.itemType 
                                            { name = Just theEvent.itemName
                                            , quantity = Just theEvent.quantity
                                            }
                        Nothing ->
                            model.lastPurchases
            in
                case event of
                    Just theEvent ->
                        ( { model | pendingEvent = Nothing, lastPurchases = lastPurchases }, saveEvent theEvent )
                    Nothing ->
                        ( model, Cmd.none )

        Msgs.RetrievedDefaults value ->
            case Decode.decodeValue decodeRememberedPurchases value of
                Ok defaults ->
                    ( { model | defaultPurchases = defaults }, Cmd.none )
                Err err ->
                    ( { model | error = ("RetrievedDefaults: " ++ err) }, Cmd.none )

        Msgs.RetrievedEventFromDatabase event ->
            case Decode.decodeValue decodeEvent event of
                Ok val ->
                    ( { model | events = (val :: model.events) }, Cmd.none )
                Err err ->
                    ( { model | error = ("RetrievedEventFromDatabase: " ++ err) }, Cmd.none )

        Msgs.RetrievedSettings value ->
            case Decode.decodeValue decodeSettings value of
                Ok settings ->
                    ( { model | settings = settings }, Cmd.none )
                Err err ->
                    ( { model | error = ("RetrievedSettings: " ++ err) }, Cmd.none )

        Msgs.ToggleShowSettings ->
            ( { model | showSettings = not model.showSettings }, Cmd.none )

        Msgs.SettingsUpdateDogName dogName ->
            let
                oldSettings = 
                    model.settings
                newSettings =
                    { oldSettings | dogName = Just dogName }
            in
                ( model, (saveSettings newSettings) )

        Msgs.SettingsUpdateUnitSystem unitSystem ->
            let
                oldSettings = 
                    model.settings
                newSettings =
                    { oldSettings | unitSystem = unitSystem }
            in
                ( model, (saveSettings newSettings) )

        Msgs.SettingsUpdateFoodPerDay amount ->
            let
                oldSettings = 
                    model.settings
                newSettings =
                    { oldSettings | dogFoodPerDay = Just amount }
            in
                ( model, (saveSettings newSettings) )

        Msgs.SettingsUpdateMedicineInterval itemType interval ->
            let
                oldSettings = 
                    model.settings
                newSettings =
                    case itemType of
                        Models.HeartwormMedicine ->
                            { oldSettings | heartwormMedicineInterval = Just interval }
                        Models.FleaTickMedicine -> 
                            { oldSettings | fleaTickMedicineInterval = Just interval }
                        _ ->
                            oldSettings
            in
                ( model, (saveSettings newSettings) )

        Msgs.DefaultsUpdateNameForItemType itemType name ->
            let
                default =
                    fetchDefaultPurchase model itemType
                
                updatedDefault =
                    { default | name = Just name }

                oldDefaults = 
                    model.defaultPurchases

                newDefaults =
                    updateDefaults oldDefaults itemType updatedDefault
            in
                ( { model | defaultPurchases = newDefaults }, (saveDefaults newDefaults) )

        Msgs.DefaultsUpdateQuantityForItemType itemType quantityString ->
            let
                default =
                    fetchDefaultPurchase model itemType
                
                strippedString =
                    String.filter isDigitOrDecimal quantityString

                newQuantity =
                    stringToFloatWithDefault quantityString 0.0

                updatedDefault = 
                    { default | quantity = Just newQuantity }

                oldDefaults = 
                    model.defaultPurchases

                newDefaults =
                    updateDefaults oldDefaults itemType updatedDefault
            in
                ( { model | defaultPurchases = newDefaults }, (saveDefaults newDefaults) )

--- PRIVATE ---

updateDefaults : RememberedPurchases -> ItemType -> RememberedPurchase -> RememberedPurchases
updateDefaults oldDefaults itemType newDefault =
    case itemType of
        Models.Food ->
            { oldDefaults | food = newDefault }
        Models.HeartwormMedicine ->
            { oldDefaults | heartwormMedicine = newDefault }
        Models.FleaTickMedicine ->
            { oldDefaults | fleaTickMedicine = newDefault }

updateLastPurchase : RememberedPurchases -> ItemType -> RememberedPurchase -> RememberedPurchases
updateLastPurchase oldLastPurchases itemType lastPurchase =
    case itemType of
        Models.Food ->
            { oldLastPurchases | food = lastPurchase }
        Models.HeartwormMedicine ->
            { oldLastPurchases | heartwormMedicine = lastPurchase }
        Models.FleaTickMedicine ->
            { oldLastPurchases | fleaTickMedicine = lastPurchase }

fetchDefaultPurchase : Model -> ItemType -> RememberedPurchase
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