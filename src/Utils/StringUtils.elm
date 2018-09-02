module Utils.StringUtils exposing (..)

import Models exposing (EventType, ItemType, UnitSystem)

--- PUBLIC ---

eventTypeToString : EventType -> String
eventTypeToString eventType =
    if eventType == Models.PurchaseEvent then
        "Purchased"
    else
        "Adminstered"

maybeItemTypeToString : Maybe ItemType -> String
maybeItemTypeToString maybeItemType =
    case maybeItemType of
        Just itemType -> (itemTypeToString itemType)
        Nothing -> ""

itemTypeToString : ItemType -> String
itemTypeToString itemType =
    case itemType of
        Models.HeartwormMedicine ->
            "Heartworm Medicine"
        Models.FleaTickMedicine ->
            "Flea/Tick Medicine"
        Models.Food ->
            "Food"

unitSystemToMassString : UnitSystem -> String
unitSystemToMassString unitSystem =
    case unitSystem of
        Models.Imperial ->
            "lb"
        Models.Metric ->
            "kg"