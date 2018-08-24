module Msgs exposing (..)

import Date exposing (Date)
import Json.Encode exposing (Value)
import Models exposing (ItemType)

type Msg
    = RequestPurchaseEvent ItemType String Float
    | RequestAdministerEvent ItemType String
    | ReceivePurchaseEvent ItemType String Float Date
    | ReceiveAdministerEvent ItemType String Date
    | GotEventFromDatabase Value