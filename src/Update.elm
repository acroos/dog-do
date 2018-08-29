module Update exposing (update)

import Char
import Commands exposing (..)
import Json.Decode as Decode
import Msgs exposing (Msg)
import Models exposing (..)
import Utils.JsonUtils exposing (decodeEvent, decodeRememberedPurchases, decodeSettings)

--- PUBLIC ---

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.UpdateNowTime date ->
            ( { model | now = date }, Cmd.none )

        Msgs.NewPendingPurchaseEvent itemType name quantity ->
            let
                purchaseEvent = (createPurchaseEvent itemType name quantity model.now)
            in
                ( { model | pendingEvent = Just purchaseEvent }, updateNowTime )

        Msgs.NewAdministerEvent itemType name ->
            let
                administerEvent = (createAdministerEvent itemType name model.now)
            in
                ( model,  batchWithTimeCmd (saveEvent administerEvent) )

        Msgs.DeletePendingEvent ->
            ( { model | pendingEvent = Nothing }, updateNowTime )

        Msgs.UpdatePendingEventItemName name ->
            let
                newPendingEvent = 
                    case model.pendingEvent of
                        Just theEvent ->
                            Just (updateEventName theEvent name)
                        Nothing ->
                            Nothing
            in                
                ( { model | pendingEvent = newPendingEvent }, updateNowTime )

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
                ( { model | pendingEvent = newPendingEvent }, updateNowTime )

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
                        ( { model | pendingEvent = Nothing, lastPurchases = lastPurchases }, batchWithTimeCmd (saveEvent theEvent) )
                    Nothing ->
                        ( model, updateNowTime )

        Msgs.RetrievedDefaults value ->
            case Decode.decodeValue decodeRememberedPurchases value of
                Ok defaults ->
                    ( { model | defaultPurchases = defaults }, updateNowTime )
                Err err ->
                    ( { model | error = ("RetrievedDefaults: " ++ err) }, updateNowTime )

        Msgs.RetrievedEventFromDatabase event ->
            case Decode.decodeValue decodeEvent event of
                Ok val ->
                    ( { model | events = (val :: model.events) }, updateNowTime )
                Err err ->
                    ( { model | error = ("RetrievedEventFromDatabase: " ++ err) }, updateNowTime )

        Msgs.RetrievedSettings value ->
            case Decode.decodeValue decodeSettings value of
                Ok settings ->
                    ( { model | settings = settings }, updateNowTime )
                Err err ->
                    ( { model | error = ("RetrievedSettings: " ++ err) }, updateNowTime )

        Msgs.ToggleShowSettings ->
            ( { model | showSettings = not model.showSettings }, updateNowTime )

        Msgs.SettingsUpdateDogName dogName ->
            let
                oldSettings = 
                    model.settings
                newSettings =
                    { oldSettings | dogName = Just dogName }
            in
                ( model, batchWithTimeCmd (saveSettings newSettings) )

        Msgs.SettingsUpdateUnitSystem unitSystem ->
            let
                oldSettings = 
                    model.settings
                newSettings =
                    { oldSettings | unitSystem = unitSystem }
            in
                ( model, batchWithTimeCmd (saveSettings newSettings) )

        Msgs.SettingsUpdateFoodPerDay amount ->
            let
                oldSettings = 
                    model.settings
                newSettings =
                    { oldSettings | dogFoodPerDay = Just amount }
            in
                ( model, batchWithTimeCmd (saveSettings newSettings) )

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
                ( model, batchWithTimeCmd (saveSettings newSettings) )

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
                ( { model | defaultPurchases = newDefaults }, batchWithTimeCmd (saveDefaults newDefaults) )

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
                ( { model | defaultPurchases = newDefaults }, batchWithTimeCmd (saveDefaults newDefaults) )

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