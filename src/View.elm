module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (src)
import Models exposing (Model)
import Msgs exposing (Msg)

view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]