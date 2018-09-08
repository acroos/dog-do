module Main exposing (..)

import Commands exposing (updateNowTime)
import Date
import Html exposing (Html)
import Models exposing (Model, initialModel)
import Msgs exposing (Msg)
import Ports exposing (..)
import Task
import Time exposing (every, hour)
import Update exposing (update)
import View exposing (view)


init : ( Model, Cmd Msg )
init =
    ( initialModel, updateNowTime )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ retrievedSettings Msgs.RetrievedSettings
        , retrievedNewEvent Msgs.RetrievedEventFromDatabase
        , retrievedDefaults Msgs.RetrievedDefaults
        , retrievedEventUpdate Msgs.DatabaseEventUpdated
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
