module Models exposing (..)

import Date exposing (Date)

type alias Model =
    { dog : Dog
    , purchaseEvents : List PurchaseEvent
    , administerEvents : List AdministerEvent
    }

initialModel : Model
initialModel =
    { dog = { name = "Fido" }
    , purchaseEvents = []
    , administerEvents = []
    }

type alias PurchaseEvent =
    { name : String
    , quantity : Float
    , date : Date
    }

type alias AdministerEvent =
    { name : String
    , date : Date
    }

type alias Dog =
    { name : String }