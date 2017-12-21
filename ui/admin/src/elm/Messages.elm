module Messages exposing (..)

import FileReader exposing (NativeFile)
import Http exposing (Error)
import Material
import Models exposing (..)
import Time exposing (..)
import WebRTC


type Msg
    = NoOp
    | WsMsg String
    | WsError UserMsg
    | WsAck String
    | WsPing Time
    | WsPong
    | ConnectOk String
    | Login
    | ShowPerspective Perspective
    | ShowNode NodeView
    | ShowTable TableView
    | ShowNodeTable NodeView TableData
    | OnLoginEmail String
    | OnLoginPassword String
    | LoginErr UserMsg
    | LoginOk UserData
    | Logout
    | LogoutOk
    | LogoutErr UserMsg
    | Nodes (List NodeData)
    | DeleteTableReplica NodeView TableData
    | AddTableReplica NodeView TableData
    | HostnameSelected String
    | MediaSelected String