module Main exposing (..)

import Commands exposing (..)
import Decoders exposing (..)
import Html exposing (..)
import Init exposing (..)
import Messages exposing (..)
import Models exposing (..)
import Rest exposing (delete, get, post, put)
import Subscriptions exposing (..)
import Update exposing (..)
import Urls exposing (..)
import Views exposing (..)


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
