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

        ShowNodes ->
            { model | state = Nodes } ! []

        ShowTables ->
            { model | state = Tables } ! []

        ShowNode n ->
            { model | state = Node, node = Just n } ! []

        ShowTable t ->
            { model | state = Table, table = Just t } ! []

        ShowNewTable ->
            case model.newTableData of
                Nothing ->
                    { model | state = NewTable, newTableData = Just newTableData } ! []

                Just t ->
                    let
                        t2 =
                            { t | name = "" }
                    in
                    { model | state = NewTable, newTableData = Just t2 } ! []

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
                | state = Nodes
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

        NodeList nodes ->
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

        NewTableNameChanged v ->
            case model.newTableData of
                Nothing ->
                    { contents = "No model to hold new table data. This is a bug. Please fix"
                    , severity = SevError
                    }
                        |> error model

                Just d ->
                    modelWithNewTableName model d v ! []

        NewTableStorageChanged v ->
            case model.newTableData of
                Nothing ->
                    { contents = "No model to hold new table data. This is a bug. Please fix"
                    , severity = SevError
                    }
                        |> error model

                Just d ->
                    case v |> toTableStorage of
                        Nothing ->
                            { contents = "Invalid table storage"
                            , severity = SevWarn
                            }
                                |> error model

                        Just s ->
                            modelWithNewTableStorage model d s ! []

        RemoveNewTableReplica r ->
            case model.newTableData of
                Nothing ->
                    { contents = "No model to hold new table data. This is a bug. Please fix"
                    , severity = SevError
                    }
                        |> error model

                Just d ->
                    modelWithoutNewTableReplica model d r ! []

        NewTableReplicaNodeChanged v ->
            { model | nodeSelection = Just v } ! []

        NewTableReplicaMediaChanged v ->
            case v |> stringToTableMedia of
                Nothing ->
                    { contents = "Invalid type of media"
                    , severity = SevWarn
                    }
                        |> error model

                Just m ->
                    { model | tableMediaSelection = Just m } ! []

        AddNewTableReplica ->
            case model |> nodeSelection of
                Nothing ->
                    { contents = "Please select a node for the replica"
                    , severity = SevWarn
                    }
                        |> error model

                Just v ->
                    withNewTableData model
                        (\t ->
                            modelWithNewTableReplica model t v ! []
                        )

        CreateTable ->
            withNewTableData model
                (\t ->
                    { model | state = CreatingTable }
                        ! [ createTable model.flags model.session t ]
                )


withNewTableData : Model -> (NewTableData -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
withNewTableData model next =
    case model.newTableData of
        Nothing ->
            { contents = "Missing 'newTableData' in model. This is a bug. Please fix."
            , severity = SevWarn
            }
                |> error model

        Just d ->
            next d


testLogin : Model -> Cmd Msg
testLogin model =
    login model.flags { email = "a@b", password = "a" } model.session


error : Model -> UserMsg -> ( Model, Cmd msg )
error model msg =
    ( { model | userMsg = Just msg }, Cmd.none )
