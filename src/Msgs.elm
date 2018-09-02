module Msgs exposing (..)

import Date exposing (Date)
import Json.Encode exposing (Value)
import Models exposing (Event, EventType, ItemType, EditableEventData, UnitSystem)

type Msg
    = UpdateNowTime Date
    | NewPendingPurchaseEvent ItemType (Maybe String) (Maybe Float)
    | NewAdministerEvent ItemType String
    | DeletePendingEvent
    | UpdatePendingEventItemType ItemType
    | UpdatePendingEventItemName String
    | UpdatePendingEventQuantity String
    | SavePendingEvent ItemType String Float
    | RetrievedDefaults Value
    | RetrievedEventFromDatabase Value
    | RetrievedSettings Value
    | SettingsUpdateDogName String
    | SettingsUpdateUnitSystem UnitSystem
    | SettingsUpdateFoodPerDay String
    | SettingsUpdateMedicineInterval ItemType String
    | DefaultsUpdateNameForItemType ItemType String
    | DefaultsUpdateQuantityForItemType ItemType String