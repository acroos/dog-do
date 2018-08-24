module Models exposing (..)

import Date exposing (Date)

type alias Model =
    { dog : Dog
    , events : List Event
    , error : String
    }

initialModel : Model
initialModel =
    { dog = { name = "Fido" }
    , events = []
    , error = ""
    }

type EventType
    = PurchaseEvent
    | AdministerEvent

type ItemType
    = HeartwormMedicine
    | FleaTickMedicine
    | Food

type alias Event =
    { eventType : EventType
    , itemType : ItemType
    , itemName : String
    , quantity : Float
    , timestamp : Date
    }

type alias Dog =
    { name : String }

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