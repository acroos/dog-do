module Msgs exposing (..)

import Date exposing (Date)
import Json.Encode exposing (Value)
import Models exposing (Event, EventType, ItemType, RememberedPurchase, UnitSystem)
import Time exposing (Time)

type Msg
    = UpdateNowTime Date
    | NewPendingPurchaseEvent ItemType String Float
    | NewAdministerEvent ItemType String
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