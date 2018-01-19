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
    | UserMessageRead
    | ShowNodes
    | ShowNode String
    | ShowTables
    | ShowNewTable
    | ShowTable TableView
    | ShowNodeTable NodeView String
    | OnLoginEmail String
    | OnLoginPassword String
    | LoginErr UserMsg
    | LoginOk UserData
    | Logout
    | LogoutOk
    | LogoutErr UserMsg
    | FetchTablesErr UserMsg
    | FetchTablesOk (List TableInfo)
    | NodeList (List NodeData)
    | DeleteTableReplica String String
    | DeleteTableReplicaErr UserMsg
    | DeleteTableReplicaOk
    | DeleteSchema NodeView
    | DeleteSchemaErr UserMsg
    | DeleteSchemaOk
    | CreateTableReplica NodeView TableData
    | CreateTableReplicaOk
    | CreateTableReplicaErr UserMsg
    | CreateTableEverywhere TableData
    | HostnameSelected String
    | MediaSelected String
    | NewTableNameChanged String
    | NewTableStorageChanged String
    | RemoveNewTableReplica TableReplicaData
    | NewTableReplicaNodeChanged String
    | NewTableReplicaMediaChanged String
    | AddNewTableReplica
    | CreateSchema NodeView
    | CreateSchemaErr UserMsg
    | CreateSchemaOk
    | CreateTable
    | CreateTableErr UserMsg
    | CreateTableOk
    | CreateTableFullyOk
    | ToggleNodeDb NodeData
    | StartNodeDbOk HostData
    | StopNodeDbOk HostData
    | ShowEvents
    | EventsOk (List EventData)
    | ClearEvents
    | ClearEventsOk
    | ShowBackups
    | BackupsOk (List BackupData)
    | ShowNewBackup
    | NewBackupNameChanged String
    | CreateBackup
    | CreateBackupOk
