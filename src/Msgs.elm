module Msgs exposing (..)

import Date exposing (Date)
import Json.Encode exposing (Value)
import Models exposing (Event, ItemType, RememberedPurchase, UnitSystem)

type Msg
    = RequestPurchaseEvent ItemType String Float
    | RequestAdministerEvent ItemType String
    | ReceivePurchaseEvent ItemType String Float Date
    | ReceiveAdministerEvent ItemType String Date
    | DeletePendingEvent
    | UpdatePendingEventItemName String
    | UpdatePendingEventQuantity String
    | SavePendingEvent
    | GotEventFromDatabase Value
    | ToggleShowSettings
    | SettingsUpdateUnitSystem UnitSystem
    | SettingsUpdateDefaultsName ItemType String
    | SettingsUpdateDefaultsQuantity ItemType String