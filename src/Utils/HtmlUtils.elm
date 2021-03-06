module Utils.HtmlUtils exposing (empty, onChange, onFocusOut)

import Html exposing (..)
import Html.Events exposing (on, targetValue)
import Json.Decode

--- PUBLIC --- 

empty : Html msg
empty =
  text ""

onChange : msg -> Attribute msg
onChange message =
  on "change" (Json.Decode.succeed message)

onFocusOut : (String -> msg) -> Attribute msg
onFocusOut tagger =
  on "focusout" (Json.Decode.map tagger targetValue)
