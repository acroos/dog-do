port module Ports exposing (..)

-- Need to import Json.Decode so the decode JS gets emitted before the ports JS
import Json.Decode
import Json.Encode exposing (Value)

port saveEvent : Value -> Cmd msg
port saveSettings : Value -> Cmd msg
port saveDefaults : Value -> Cmd msg
port updateEvent : Value -> Cmd msg

port retrievedSettings : (Value -> msg) -> Sub msg
port retrievedDefaults : (Value -> msg) -> Sub msg
port retrievedNewEvent : (Value -> msg) -> Sub msg
port retrievedEventUpdate : (Value -> msg) -> Sub msg