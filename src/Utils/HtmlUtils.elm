module Utils.HtmlUtils exposing (onChange, onFocusOut)

import Html exposing (..)
import Html.Events exposing (on, targetValue)
import Json.Decode

--- PUBLIC --- 

onChange : msg -> Attribute msg
onChange message =
  on "change" (Json.Decode.succeed message)

onFocusOut : (String -> msg) -> Attribute msg
onFocusOut tagger =
  on "focusout" (Json.Decode.map tagger targetValue)
