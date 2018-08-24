module Update exposing (update)

import Commands exposing (saveAdministerEvent, savePurchaseEvent)
import Date exposing (Date)
import Msgs exposing (Msg)
import Models exposing (Model)
import Task exposing (Task)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.RequestPurchaseEvent name quantity ->
            ( model, Task.perform (Msgs.ReceivePurchaseEvent name quantity) Date.now)
        Msgs.RequestAdministerEvent name ->
            ( model, Task.perform (Msgs.ReceiveAdministerEvent name) Date.now)
        Msgs.ReceivePurchaseEvent name quantity date ->
            let
                purchaseEvent =
                    { name = name
                    , quantity = quantity
                    , date = date
                    }
            in
                ( { model | purchaseEvents = (purchaseEvent :: model.purchaseEvents) }, (savePurchaseEvent purchaseEvent) )
        Msgs.ReceiveAdministerEvent name date ->
            let
                administerEvent =
                    { name = name
                    , date = date
                    }
            in
                ( { model | administerEvents = (administerEvent :: model.administerEvents) }, (saveAdministerEvent administerEvent) )