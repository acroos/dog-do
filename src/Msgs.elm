module Msgs exposing (..)

import Date exposing (Date)

type Msg
    = RequestPurchaseEvent String Float
    | RequestAdministerEvent String
    | ReceivePurchaseEvent String Float Date
    | ReceiveAdministerEvent String Date