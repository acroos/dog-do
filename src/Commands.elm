module Commands exposing (saveAdministerEvent, savePurchaseEvent)

import JsonUtils exposing (encodeAdminsterEvent, encodePurchaseEvent)
import Models exposing (AdministerEvent, PurchaseEvent)
import Msgs exposing (Msg)
import Ports

saveAdministerEvent : AdministerEvent -> Cmd Msg
saveAdministerEvent event =
    let
        jsonValue = (encodeAdminsterEvent event)
    in
        Ports.storeAdministerEvent jsonValue

savePurchaseEvent : PurchaseEvent -> Cmd Msg
savePurchaseEvent event =
    let
        jsonValue = (encodePurchaseEvent event)
    in
        Ports.storePurchaseEvent jsonValue
