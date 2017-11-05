module Subscriptions exposing (..)

import Messages exposing (..)
import Models exposing (..)
import Time exposing (..)
import Urls exposing (..)
import WebRTC exposing (..)
import WebSocket exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (Time.second * 15) WsPing
        , WebSocket.listen model.flags.ws WsMsg
        , WebRTC.ice Ice
        ]
