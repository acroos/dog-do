module Models exposing (..)

import Date exposing (Date)

type alias Model =
    { settings: Settings
    , events : List Event
    , error : String
    , pendingEvent : PendingEvent
    , defaultPurchases : Defaults
    , now : Date
    , eventToEdit : Maybe Event
    }

initialModel : Model
initialModel =
    { settings = emptySettings
    , events = []
    , error = ""
    , pendingEvent = emptyPendingEvent
    , defaultPurchases = emptyDefaults
    , now = Date.fromTime 0
    , eventToEdit = Nothing
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
    { id : Maybe Int
    , eventType : EventType
    , itemType : ItemType
    , itemName : String
    , quantity : Float
    , timestamp : Date
    }

type alias PendingEvent =
    { itemType : Maybe ItemType
    , editableData : EditableEventData
    , saveAsDefaults : Bool
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

createAdministerEvent : ItemType -> String -> Date -> Event
createAdministerEvent itemType itemName date =
    { eventType = AdministerEvent
    , itemType = itemType
    , itemName = itemName
    , quantity = 1.0
    , timestamp = date
    , id = Nothing
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
    , id = currentEvent.id
    }

updateEventQuantity : Event -> Float -> Event
updateEventQuantity currentEvent newQuantity =
    { eventType = currentEvent.eventType
    , itemType = currentEvent.itemType
    , itemName = currentEvent.itemName
    , quantity = newQuantity
    , timestamp = currentEvent.timestamp
    , id = currentEvent.id
    }

emptyDefaults : Defaults
emptyDefaults =
    { food = emptyEditableEventData
    , heartwormMedicine = emptyEditableEventData
    , fleaTickMedicine = emptyEditableEventData
    }

emptyPendingEvent : PendingEvent
emptyPendingEvent =
    { itemType = Nothing
    , editableData = emptyEditableEventData
    , saveAsDefaults = False
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