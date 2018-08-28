module Commands exposing (..)

import Json.Encode exposing (string)
import Models exposing (Event, RememberedPurchases, UnitSystem)
import Msgs exposing (Msg)
import Ports
import Utils.JsonUtils exposing (encodeEvent, encodeRememberedPurchases)

saveEvent : Event -> Cmd Msg
saveEvent event =
    Ports.saveEvent (encodeEvent event)

saveDogName : String -> Cmd Msg
saveDogName dogName =
    Ports.saveDogName (string dogName)

saveUnitSystem : UnitSystem -> Cmd Msg
saveUnitSystem unitSystem =
    Ports.saveUnitSystem (string (toString unitSystem))

saveDefaults : RememberedPurchases -> Cmd Msg
saveDefaults defaults =
    Ports.saveDefaults (encodeRememberedPurchases defaults)