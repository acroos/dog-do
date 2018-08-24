module Commands exposing (saveEvent)

import JsonUtils exposing (encodeEvent)
import Models exposing (Event)
import Msgs exposing (Msg)
import Ports

saveEvent : Event -> Cmd Msg
saveEvent event =
    Ports.saveEvent (encodeEvent event)