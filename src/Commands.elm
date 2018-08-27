module Commands exposing (saveEvent)

import Models exposing (Event)
import Msgs exposing (Msg)
import Ports
import Utils.JsonUtils exposing (encodeEvent)

saveEvent : Event -> Cmd Msg
saveEvent event =
    Ports.saveEvent (encodeEvent event)