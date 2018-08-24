module JsonUtils exposing ( encodeAdminsterEvent
                          , encodePurchaseEvent
                          )

import DateUtils exposing (jsonEncodeDate)
import Json.Encode exposing (Value, float, object, string)
import Models exposing (AdministerEvent, PurchaseEvent)

encodeAdminsterEvent : AdministerEvent -> Value
encodeAdminsterEvent event =
    object(
        [ ("name", (string event.name))
        , ("date", (jsonEncodeDate event.date))
        ])

encodePurchaseEvent : PurchaseEvent -> Value
encodePurchaseEvent event =
    object(
        [ ("name", (string event.name))
        , ("quantity", (float event.quantity))
        , ("date", (jsonEncodeDate event.date))
        ])