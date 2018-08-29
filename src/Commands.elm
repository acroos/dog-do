module Commands exposing (..)

import Date exposing (now)
import Models exposing (Event, RememberedPurchases, Settings)
import Msgs exposing (Msg)
import Ports
import Task exposing (perform)
import Utils.JsonUtils exposing (encodeEvent, encodeRememberedPurchases, encodeSettings)

batchWithTimeCmd : Cmd Msg -> Cmd Msg
batchWithTimeCmd cmd =
    Cmd.batch [ updateNowTime, cmd ]

saveEvent : Event -> Cmd Msg
saveEvent event =
    Ports.saveEvent (encodeEvent event)

saveSettings : Settings -> Cmd Msg
saveSettings settings =
    Ports.saveSettings (encodeSettings settings)

saveDefaults : RememberedPurchases -> Cmd Msg
saveDefaults defaults =
    Ports.saveDefaults (encodeRememberedPurchases defaults)

updateNowTime : Cmd Msg
updateNowTime =
    perform Msgs.UpdateNowTime now