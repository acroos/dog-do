module Main exposing (..)

import Html exposing (Html)
import Models exposing (Model, initialModel)
import Msgs exposing (Msg)
import Ports exposing (..)
import Update exposing (update)
import View exposing (view)


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ retrievedSettings Msgs.RetrievedSettings
        , retrievedEventFromDatabase Msgs.RetrievedEventFromDatabase
        , retrievedDefaults Msgs.RetrievedDefaults
        ]


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
