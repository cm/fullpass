module Update exposing (..)

import Commands exposing (..)
import Decoders exposing (..)
import Init exposing (..)
import Material
import Messages exposing (..)
import Models exposing (..)
import Task exposing (Task)
import WebRTC exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        WsMsg str ->
            case wsResult str of
                Ok msg2 ->
                    update msg2 model

                Err e ->
                    update (WsError (stringAsError e)) model

        WsAck action ->
            model ! []

        WsPing _ ->
            model ! [ ping model.flags ]

        WsError e ->
            e |> error model

        WsPong ->
            model ! []

        ConnectOk sid ->
            let
                m2 =
                    sid |> modelWithSession model.flags
            in
            m2 ! [ testLogin m2 ]

        ShowPerspective p ->
            case p of
                Servers ->
                    { model | perspective = p, node = Nothing } ! []

                _ ->
                    { model | perspective = p, table = Nothing } ! []

        ShowNode n ->
            { model | node = Just n } ! []

        ShowTable t ->
            { model | table = Just t } ! []

        ShowNodeTable view t ->
            let
                node2 =
                    { view | table = Just t }
            in
            { model
                | node = Just node2
                , hostname = Nothing
                , media = Nothing
            }
                ! []

        OnLoginEmail v ->
            { model | loginData = loginDataWithEmail model.loginData v } ! []

        OnLoginPassword v ->
            { model | loginData = loginDataWithPassword model.loginData v } ! []

        Login ->
            model ! [ login model.flags model.loginData model.session ]

        LoginOk u ->
            { model
                | state = SignedIn
                , loginData = newLoginData
                , user = Just u
            }
                ! [ fetch_nodes model ]

        LoginErr e ->
            e |> error model

        Logout ->
            { model
                | state = SignedOut
                , loginData = newLoginData
            }
                ! [ logout model.flags model.session ]

        LogoutOk ->
            newModel model.flags
                |> update (ConnectOk model.session)

        LogoutErr e ->
            e |> error model

        Nodes nodes ->
            { model
                | nodes = indexedNodes model nodes
                , tables = indexedTables model nodes
            }
                ! []

        DeleteTableReplica v t ->
            let
                v2 =
                    { v | state = DeletingReplica }
            in
            { model | node = Just v2 }
                ! [ deleteTableReplica model.flags model.session v.node.info.hostname t.name ]

        AddTableReplica v t ->
            case model.hostname of
                Nothing ->
                    { contents = "Please select a hostname"
                    , severity = SevError
                    }
                        |> error model

                Just h ->
                    case t.name of
                        "schema" ->
                            let
                                v2 =
                                    { v | state = AddingReplica }
                            in
                            { model | node = Just v2 }
                                ! [ addTableReplica model.flags model.session v.node.info.hostname t.name h "both" ]

                        _ ->
                            case model.media of
                                Nothing ->
                                    { contents = "Please select a media"
                                    , severity = SevError
                                    }
                                        |> error model

                                Just m ->
                                    let
                                        v2 =
                                            { v | state = AddingReplica }
                                    in
                                    { model | node = Just v2 }
                                        ! [ addTableReplica model.flags model.session v.node.info.hostname t.name h m ]

        HostnameSelected h ->
            { model | hostname = Just h } ! []

        MediaSelected m ->
            { model | media = Just m } ! []


testLogin : Model -> Cmd Msg
testLogin model =
    login model.flags { email = "a@b", password = "a" } model.session


error : Model -> UserMsg -> ( Model, Cmd msg )
error model msg =
    ( { model | userMsg = Just msg }, Cmd.none )
