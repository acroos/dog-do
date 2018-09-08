module Commands exposing (..)

import Date exposing (now)
import Models exposing (Event, Defaults, Settings)
import Msgs exposing (Msg)
import Ports
import Task exposing (perform)
import Utils.JsonUtils exposing (encodeEvent, encodeDefaults, encodeSettings)

batchWithTimeCmd : Cmd Msg -> Cmd Msg
batchWithTimeCmd cmd =
    Cmd.batch [ updateNowTime, cmd ]

saveEvent : Event -> Cmd Msg
saveEvent event =
    Ports.saveEvent (encodeEvent event)

saveSettings : Settings -> Cmd Msg
saveSettings settings =
    Ports.saveSettings (encodeSettings settings)

saveDefaults : Defaults -> Cmd Msg
saveDefaults defaults =
    Ports.saveDefaults (encodeDefaults defaults)

updateEvent : Event -> Cmd Msg
updateEvent event =
    Ports.updateEvent (encodeEvent event)

updateNowTime : Cmd Msg
updateNowTime =
    perform Msgs.UpdateNowTime now