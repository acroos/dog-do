module Models exposing (..)

import Date exposing (Date)

type alias Model =
    { settings: Settings
    , events : List Event
    , error : String
    , pendingEvent : Maybe Event
    , defaultPurchases : Defaults
    , now : Date
    }

initialModel : Model
initialModel =
    { settings = emptySettings
    , events = []
    , error = ""
    , pendingEvent = Nothing
    , defaultPurchases = emptyDefaults
    , now = Date.fromTime 0
    }

type EventType
    = PurchaseEvent
    | AdministerEvent

type ItemType
    = HeartwormMedicine
    | FleaTickMedicine
    | Food

type UnitSystem
    = Metric
    | Imperial

type alias Event =
    { eventType : EventType
    , itemType : ItemType
    , itemName : String
    , quantity : Float
    , timestamp : Date
    }

type alias Defaults =
    { food : EditableEventData
    , heartwormMedicine : EditableEventData
    , fleaTickMedicine : EditableEventData
    }

type alias EditableEventData =
    { name : Maybe String
    , quantity : Maybe Float
    }

type alias Settings =
    { dogName : Maybe String
    , unitSystem : UnitSystem
    , dogFoodPerDay : Maybe Float
    , heartwormMedicineInterval : Maybe Int
    , fleaTickMedicineInterval : Maybe Int
    }

createPurchaseEvent : ItemType -> String -> Float -> Date -> Event
createPurchaseEvent itemType itemName quantity date =
    { eventType = PurchaseEvent
    , itemType = itemType
    , itemName = itemName
    , quantity = quantity
    , timestamp = date
    }

createAdministerEvent : ItemType -> String -> Date -> Event
createAdministerEvent itemType itemName date =
    { eventType = AdministerEvent
    , itemType = itemType
    , itemName = itemName
    , quantity = 1.0
    , timestamp = date
    }

unitSystemFromString : String -> UnitSystem
unitSystemFromString string =
    if string == (toString Imperial) then
        Imperial
    else
        Metric

updateEventName : Event -> String -> Event
updateEventName currentEvent newName =
    { eventType = currentEvent.eventType
    , itemType = currentEvent.itemType
    , itemName = newName
    , quantity = currentEvent.quantity
    , timestamp = currentEvent.timestamp
    }

updateEventQuantity : Event -> Float -> Event
updateEventQuantity currentEvent newQuantity =
    { eventType = currentEvent.eventType
    , itemType = currentEvent.itemType
    , itemName = currentEvent.itemName
    , quantity = newQuantity
    , timestamp = currentEvent.timestamp
    }

emptyDefaults : Defaults
emptyDefaults =
    { food = emptyEditableEventData
    , heartwormMedicine = emptyEditableEventData
    , fleaTickMedicine = emptyEditableEventData
    }

emptyEditableEventData : EditableEventData
emptyEditableEventData =
    { name = Nothing, quantity = Nothing }

emptySettings : Settings
emptySettings =
    { dogName = Nothing
    , unitSystem = Metric
    , dogFoodPerDay = Nothing
    , heartwormMedicineInterval = Nothing
    , fleaTickMedicineInterval = Nothing
    }