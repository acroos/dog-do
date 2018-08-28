module Commands exposing (..)

import Models exposing (Event, RememberedPurchases, Settings)
import Msgs exposing (Msg)
import Ports
import Utils.JsonUtils exposing (encodeEvent, encodeRememberedPurchases, encodeSettings)

saveEvent : Event -> Cmd Msg
saveEvent event =
    Ports.saveEvent (encodeEvent event)

saveSettings : Settings -> Cmd Msg
saveSettings settings =
    Ports.saveSettings (encodeSettings settings)

saveDefaults : RememberedPurchases -> Cmd Msg
saveDefaults defaults =
    Ports.saveDefaults (encodeRememberedPurchases defaults)