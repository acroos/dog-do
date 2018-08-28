port module Ports exposing (..)

-- Need to import Json.Decode so the decode JS gets emitted before the ports JS
import Json.Decode
import Json.Encode exposing (Value)

port saveEvent : Value -> Cmd msg
port saveDogName : Value -> Cmd msg
port saveUnitSystem : Value -> Cmd msg
port saveDefaults : Value -> Cmd msg

port gotDogName : (String -> msg) -> Sub msg
port gotUnitSystem : (String -> msg) -> Sub msg
port gotDefaults : (Value -> msg) -> Sub msg
port gotEventFromDatabase : (Value -> msg) -> Sub msg