module Views.Shared exposing (divCol, divRow)

import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs exposing (Msg)

--- PUBLIC ---

divRow :  List (Html Msg) -> Html Msg
divRow columns =
    div [ class "row" ] columns

divCol : List (Html Msg) -> Html Msg
divCol content =
    div [ class "col" ] content