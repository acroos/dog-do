port module Ports exposing (gotEventFromDatabase, saveEvent)

import Json.Encode exposing (Value)

port saveEvent : Value -> Cmd msg

port gotEventFromDatabase : (Value -> msg) -> Sub msg