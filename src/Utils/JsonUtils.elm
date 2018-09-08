module Utils.JsonUtils exposing 
    ( decodeEvent
    , encodeEvent
    , decodeDefaults
    , encodeDefaults
    , decodeSettings
    , encodeSettings
    )

import Date exposing (Date)
import Json.Decode as Decode
import Json.Encode exposing (..)
import Maybe exposing (map, withDefault)
import Models exposing (..)
import Utils.DateUtils exposing (toIso8601String)

--- PUBLIC ---

encodeEvent : Event -> Value
encodeEvent event =
    let
        idValue =
            Maybe.map int event.id
            |> Maybe.withDefault null
    in
        object
            [ ("id", idValue)
            , ("eventType", (string (eventTypeToString event.eventType)))
            , ("itemType", (string (itemTypeToString event.itemType)))
            , ("itemName", (string event.itemName))
            , ("quantity", (float event.quantity))
            , ("timestamp", (encodeDate event.timestamp))
            ]

decodeEvent : Decode.Decoder Event
decodeEvent =
    Decode.map6 Event
        (Decode.maybe (Decode.field "id" Decode.int))
        (Decode.field "eventType" eventTypeDecoder)
        (Decode.field "itemType" itemTypeDecoder)
        (Decode.field "itemName" Decode.string)
        (Decode.field "quantity" Decode.float)
        (Decode.field "timestamp" dateDecoder)

encodeDefaults : Defaults -> Value
encodeDefaults remembered =
    object
        [ ("food", (encodeEditableEventData remembered.food))
        , ("heartwormMedicine", (encodeEditableEventData remembered.heartwormMedicine))
        , ("fleaTickMedicine", (encodeEditableEventData remembered.fleaTickMedicine))
        ]

decodeDefaults : Decode.Decoder Defaults
decodeDefaults =
    Decode.map3 Defaults
        (Decode.field "food" decodeEditableEventData)
        (Decode.field "heartwormMedicine" decodeEditableEventData)
        (Decode.field "fleaTickMedicine" decodeEditableEventData)

encodeSettings : Settings -> Value
encodeSettings settings =
    let
        dogName =
            case settings.dogName of
                Just name ->
                    if name == "" then
                         null
                    else
                        string name
                Nothing ->
                    null
        dogFoodPerDay = 
            case settings.dogFoodPerDay of
                Just value ->
                    if value == 0 then
                        null
                    else
                        float value
                Nothing ->
                    null
        heartwormMedicineInterval = 
            case settings.heartwormMedicineInterval of
                Just value ->
                    if value == 0 then
                        null
                    else
                        int value
                Nothing ->
                    null
        fleaTickMedicineInterval = 
            case settings.fleaTickMedicineInterval of
                Just value ->
                    if value == 0 then
                        null
                    else
                        int value
                Nothing ->
                    null
    in
        object
            [ ("dogName", dogName)
            , ("unitSystem", string (toString settings.unitSystem))
            , ("dogFoodPerDay", dogFoodPerDay)
            , ("heartwormMedicineInterval", heartwormMedicineInterval)
            , ("fleaTickMedicineInterval", fleaTickMedicineInterval)
            ]

decodeSettings : Decode.Decoder Settings
decodeSettings =
    Decode.map5 Settings
        (Decode.maybe (Decode.field "dogName" Decode.string))
        (Decode.field "unitSystem" decodeUnitSystem)
        (Decode.maybe (Decode.field "dogFoodPerDay" Decode.float))
        (Decode.maybe (Decode.field "heartwormMedicineInterval" Decode.int))
        (Decode.maybe (Decode.field "fleaTickMedicineInterval" Decode.int))

--- PRIVATE ---

decodeEditableEventData : Decode.Decoder EditableEventData
decodeEditableEventData =
    Decode.map2 EditableEventData
        (Decode.maybe (Decode.field "name" Decode.string))
        (Decode.maybe (Decode.field "quantity" Decode.float))

encodeEditableEventData : EditableEventData -> Value
encodeEditableEventData remembered =
    let
        nameVal = 
            case remembered.name of
                Just name ->
                    if name == "" then
                        null
                    else
                        string name
                Nothing ->
                    null

        quantityVal =
            case remembered.quantity of
                Just quantity ->
                    if quantity == 0 then
                        null
                    else
                        float quantity
                Nothing ->
                    null
    in    
        object
            [ ("name", nameVal)
            , ("quantity", quantityVal)
            ]

eventTypeDecoder : Decode.Decoder EventType
eventTypeDecoder =
    let
        convert : String -> Decode.Decoder EventType
        convert raw =
            Decode.succeed (eventTypeFromString raw)
    in
        Decode.string |> Decode.andThen convert

eventTypeFromString : String -> EventType
eventTypeFromString eventType =
    if eventType == purchaseEventTypeString then
        Models.PurchaseEvent
    else
        Models.AdministerEvent

itemTypeFromString : String -> ItemType
itemTypeFromString itemType =
    if itemType == heartwormMedicineItemTypeString then
        Models.HeartwormMedicine
    else if itemType == fleaTickMedicineItemTypeString then
        Models.FleaTickMedicine
    else
        Models.Food

itemTypeDecoder : Decode.Decoder ItemType
itemTypeDecoder =
    let 
        convert : String -> Decode.Decoder ItemType
        convert raw = 
            Decode.succeed (itemTypeFromString raw)
    in
    Decode.string |> Decode.andThen convert

encodeDate : Date -> Value
encodeDate date =
    string (toIso8601String date)

dateDecoder : Decode.Decoder Date
dateDecoder =
    let
        convert : String -> Decode.Decoder Date
        convert raw =
            case Date.fromString raw of
                Ok date ->
                    Decode.succeed date

                Err error ->
                    Decode.fail error
    in
        Decode.string |> Decode.andThen convert

eventTypeToString : EventType -> String
eventTypeToString eventType =
    case eventType of
        Models.PurchaseEvent ->
            purchaseEventTypeString
        Models.AdministerEvent ->
            administerEventTypeString

itemTypeToString : ItemType -> String
itemTypeToString itemType =
    case itemType of
        Models.HeartwormMedicine ->
            heartwormMedicineItemTypeString
        Models.FleaTickMedicine ->
            fleaTickMedicineItemTypeString
        Models.Food ->
            foodItemTypeString

decodeUnitSystem : Decode.Decoder UnitSystem
decodeUnitSystem =
    let
        convert : String -> Decode.Decoder UnitSystem
        convert raw = 
            Decode.succeed (unitSystemFromString raw)
    in
        Decode.string |> Decode.andThen convert





--- STRING VALUES FOR ENCODING/DECODING ---

purchaseEventTypeString : String
purchaseEventTypeString =
    "purchase"

administerEventTypeString : String
administerEventTypeString =
    "administer"

heartwormMedicineItemTypeString : String
heartwormMedicineItemTypeString =
    "heartworm"

fleaTickMedicineItemTypeString : String
fleaTickMedicineItemTypeString =
    "fleatick"

foodItemTypeString : String
foodItemTypeString =
    "food"