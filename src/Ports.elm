port module Ports exposing (gotEventFromDatabase, saveEvent)

import Json.Encode exposing (Value)

port saveEvent : Value -> Cmd msg
port saveDogName : Value -> Cmd msg
port saveUnitSystem : Value -> Cmd msg
port saveDefaults : Value -> Cmd msg

port gotEventFromDatabase : (Value -> msg) -> Sub msg