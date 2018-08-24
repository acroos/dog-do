module JsonUtils exposing (decodeEvent, encodeEvent)

import Date exposing (Date)
import DateUtils exposing (toIso8601String)
import Json.Decode as Decode
import Json.Encode exposing (Value, float, object, string)
import Models exposing (Event, EventType, ItemType)

encodeEvent : Event -> Value
encodeEvent event =
    object(
        [ ("eventType", (string (eventTypeToString event.eventType)))
        , ("itemType", (string (itemTypeToString event.itemType)))
        , ("itemName", (string event.itemName))
        , ("quantity", (float event.quantity))
        , ("timestamp", (encodeDate event.timestamp))
        ])

decodeEvent : Decode.Decoder Event
decodeEvent =
    Decode.map5 Event
        (Decode.field "eventType" eventTypeDecoder)
        (Decode.field "itemType" itemTypeDecoder)
        (Decode.field "itemName" Decode.string)
        (Decode.field "quantity" Decode.float)
        (Decode.field "timestamp" dateDecoder)

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