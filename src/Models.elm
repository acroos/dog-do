module Models exposing (..)

import Date exposing (Date)

type alias Model =
    { dog : Dog
    , unitSystem : UnitSystem
    , events : List Event
    , error : String
    , showSettings : Bool
    , pendingEvent : Maybe Event
    , lastPurchases : RememberedPurchases
    , defaultPurchases : RememberedPurchases
    }

initialModel : Model
initialModel =
    { dog = { name = Nothing }
    , unitSystem = Metric
    , events = []
    , error = ""
    , showSettings = False
    , pendingEvent = Nothing
    , lastPurchases = emptyRememberedPurchases
    , defaultPurchases = emptyRememberedPurchases
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

type alias Dog =
    { name : Maybe String }

type alias RememberedPurchases =
    { food : RememberedPurchase
    , heartwormMedicine : RememberedPurchase
    , fleaTickMedicine : RememberedPurchase
    }

type alias RememberedPurchase =
    { name : Maybe String
    , quantity : Maybe Float
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

emptyRememberedPurchases : RememberedPurchases
emptyRememberedPurchases =
    { food = emptyRememberedPurchase
    , heartwormMedicine = emptyRememberedPurchase
    , fleaTickMedicine = emptyRememberedPurchase
    }

emptyRememberedPurchase : RememberedPurchase
emptyRememberedPurchase =
    { name = Nothing, quantity = Nothing }