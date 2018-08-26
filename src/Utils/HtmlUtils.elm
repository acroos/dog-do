module Utils.HtmlUtils exposing (onFocusOut)

import Html exposing (..)
import Html.Events exposing (on, targetValue)
import Json.Decode

onFocusOut : (String -> msg) -> Attribute msg
onFocusOut tagger =
  on "focusout" (Json.Decode.map tagger targetValue)
