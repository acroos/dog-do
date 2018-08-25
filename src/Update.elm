module Update exposing (update)

import Commands exposing (saveEvent)
import Date exposing (Date)
import Json.Decode as Decode
import JsonUtils exposing (decodeEvent)
import Msgs exposing (Msg)
import Models exposing (..)
import Task exposing (Task)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.RequestPurchaseEvent itemType name quantity ->
            ( model, Task.perform (Msgs.ReceivePurchaseEvent itemType name quantity) Date.now)

        Msgs.RequestAdministerEvent itemType name ->
            ( model, Task.perform (Msgs.ReceiveAdministerEvent itemType name) Date.now)

        Msgs.ReceivePurchaseEvent itemType name quantity date ->
            let
                purchaseEvent = (createPurchaseEvent itemType name quantity date)
            in
                ( model, (saveEvent purchaseEvent) )

        Msgs.ReceiveAdministerEvent itemType name date ->
            let
                administerEvent = (createAdministerEvent itemType name date)
            in
                ( model, (saveEvent administerEvent) )

        Msgs.GotEventFromDatabase event ->
            case Decode.decodeValue decodeEvent event of
                Ok val ->
                    ( { model | events = (val :: model.events) }, Cmd.none )
                Err err ->
                    ( { model | error = err }, Cmd.none )

        Msgs.ToggleShowSettings ->
            ( { model | showSettings = not model.showSettings }, Cmd.none )

        Msgs.SaveSettings ->
            ( { model | showSettings = False }, Cmd.none )