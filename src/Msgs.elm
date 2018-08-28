module Msgs exposing (..)

import Date exposing (Date)
import Json.Encode exposing (Value)
import Models exposing (Event, ItemType, RememberedPurchase, UnitSystem)

type Msg
    = NewPurchaseEventRequestTimestamp ItemType String Float
    | NewAdministerEventRequestTimestamp ItemType String
    | NewPurchaseEventReceiveTimestamp ItemType String Float Date
    | NewAdministerEventReceiveTimestamp ItemType String Date
    | DeletePendingEvent
    | UpdatePendingEventItemName String
    | UpdatePendingEventQuantity String
    | SavePendingEvent
    | RetrievedDefaults Value
    | RetrievedEventFromDatabase Value
    | RetrievedSettings Value
    | ToggleShowSettings
    | SettingsUpdateDogName String
    | SettingsUpdateUnitSystem UnitSystem
    | SettingsUpdateFoodPerDay Float
    | SettingsUpdateMedicineInterval ItemType Int
    | DefaultsUpdateNameForItemType ItemType String
    | DefaultsUpdateQuantityForItemType ItemType String