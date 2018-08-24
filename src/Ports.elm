port module Ports exposing (storeAdministerEvent, storePurchaseEvent)

import Json.Encode exposing (Value)

port storeAdministerEvent : Value -> Cmd msg
port storePurchaseEvent : Value -> Cmd msg