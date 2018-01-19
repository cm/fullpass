module Decoders exposing (..)

import Init exposing (..)
import Json.Decode as Decode exposing (..)
import Messages exposing (..)
import Models exposing (..)


wsResult : String -> Result String Msg
wsResult str =
    decodeString wsDecoder str


wsDecoder : Decoder Msg
wsDecoder =
    field "status" Decode.string
        |> andThen statusDecoder


statusDecoder : String -> Decoder Msg
statusDecoder status =
    case status of
        "error" ->
            field "action" Decode.string
                |> andThen actionErrorDecoder

        "not_found" ->
            field "action" Decode.string
                |> andThen actionNotFoundDecoder

        "invalid" ->
            field "action" Decode.string
                |> andThen actionInvalidDecoder

        "ack" ->
            field "action" Decode.string
                |> andThen actionAckDecoder

        "ok" ->
            field "action" Decode.string
                |> andThen actionOkDecoder

        _ ->
            ("Unknown status: " ++ status)
                |> Decode.fail


actionErrorDecoder : String -> Decoder Msg
actionErrorDecoder action =
    case action of
        "login" ->
            Decode.map LoginErr
                (field "data" errorDecoder)

        "logout" ->
            Decode.map LogoutErr
                (field "data" errorDecoder)

        "create_table" ->
            Decode.map CreateTableErr
                (field "data" errorDecoder)

        "delete_table_replica" ->
            Decode.map DeleteTableReplicaErr
                (field "data" errorDecoder)

        "create_table_replica" ->
            Decode.map CreateTableReplicaErr
                (field "data" errorDecoder)

        "create_schema_replica" ->
            Decode.map CreateTableReplicaErr
                (field "data" errorDecoder)

        "delete_schema" ->
            Decode.map DeleteSchemaErr
                (field "data" errorDecoder)

        "create_schema" ->
            Decode.map CreateSchemaErr
                (field "data" errorDecoder)

        "cluster_tables" ->
            Decode.map FetchTablesErr
                (field "data" errorDecoder)

        _ ->
            ("Unsupported action: " ++ action ++ " when decoding error")
                |> Decode.fail


actionInvalidDecoder : String -> Decoder Msg
actionInvalidDecoder action =
    case action of
        "login" ->
            Decode.map LoginErr
                (field "data" invalidDecoder)

        "logout" ->
            Decode.map LogoutErr
                (field "data" errorDecoder)

        _ ->
            ("Unsupported action: " ++ action ++ " when decoding error")
                |> Decode.fail


actionNotFoundDecoder : String -> Decoder Msg
actionNotFoundDecoder action =
    Decode.fail ("Not implemented: " ++ action)


actionAckDecoder : String -> Decoder Msg
actionAckDecoder a =
    Decode.succeed (WsAck a)


actionOkDecoder : String -> Decoder Msg
actionOkDecoder action =
    case action of
        "connect" ->
            Decode.map ConnectOk
                (field "data" connectDecoder)

        "login" ->
            Decode.map LoginOk
                (field "data" userDecoder)

        "logout" ->
            Decode.succeed LogoutOk

        "ping" ->
            Decode.succeed WsPong

        "nodes" ->
            Decode.map NodeList
                (field "data" nodesDecoder)

        "cluster_tables" ->
            Decode.map FetchTablesOk
                (field "data" (tableInfoDecoder |> Decode.list))

        "create_table" ->
            Decode.succeed CreateTableOk

        "create_table_fully" ->
            Decode.succeed CreateTableFullyOk

        "delete_table_replica" ->
            Decode.succeed DeleteTableReplicaOk

        "create_table_replica" ->
            Decode.succeed CreateTableReplicaOk

        "create_schema_replica" ->
            Decode.succeed CreateTableReplicaOk

        "delete_schema" ->
            Decode.succeed DeleteSchemaOk

        "create_schema" ->
            Decode.succeed CreateSchemaOk

        "stop_db" ->
            Decode.map StopNodeDbOk
                (field "data" hostDecoder)

        "start_db" ->
            Decode.map StartNodeDbOk
                (field "data" hostDecoder)

        "cluster_events" ->
            Decode.map EventsOk
                (field "data" eventsDecoder)

        "cluster_clear_events" ->
            Decode.succeed ClearEventsOk

        "backups" ->
            Decode.map BackupsOk
                (field "data" backupsDecoder)

        "create_backup" ->
            Decode.succeed CreateBackupOk

        _ ->
            ("Unsupported action: " ++ action ++ " when decoding ok")
                |> Decode.fail


connectDecoder : Decoder String
connectDecoder =
    field "session" Decode.string


errorDecoder : Decoder UserMsg
errorDecoder =
    Decode.map2 UserMsg
        (field "reason" Decode.string)
        (Decode.succeed SevError)


invalidDecoder : Decoder UserMsg
invalidDecoder =
    Decode.map2 UserMsg
        (field "invalid" Decode.string)
        (Decode.succeed SevWarn)


userDecoder : Decoder UserData
userDecoder =
    Decode.map UserData
        (field "first" Decode.string)


nodesDecoder : Decoder (List NodeData)
nodesDecoder =
    Decode.list nodeDecoder


nodeDecoder : Decoder NodeData
nodeDecoder =
    Decode.map4 NodeData
        (field "info" infoDataDecoder)
        (field "cluster" clusterDataDecoder)
        (field "perf" perfDataDecoder)
        (field "db" dbDataDecoder)


tableInfoDecoder : Decoder TableInfo
tableInfoDecoder =
    Decode.map3 TableInfo
        (field "name" Decode.string)
        (field "type" Decode.string)
        (field "media" Decode.string)


infoDataDecoder : Decoder InfoData
infoDataDecoder =
    Decode.map2 InfoData
        (field "hostname" Decode.string)
        (field "ips" ipsDecoder)


clusterDataDecoder : Decoder ClusterData
clusterDataDecoder =
    Decode.map2 ClusterData
        (field "health" Decode.string)
        (field "peers" (Decode.list Decode.string))


dbDataDecoder : Decoder DbData
dbDataDecoder =
    Decode.map2 DbData
        (field "started" Decode.bool)
        (field "tables" (Decode.list tableDataDecoder))


tableDataDecoder : Decoder TableData
tableDataDecoder =
    Decode.map3 TableData
        (field "info" tableInfoDecoder)
        (field "size" tableSizeDecoder)
        (field "copies" tableCopiesDecoder)


tableSizeDecoder : Decoder TableSizeData
tableSizeDecoder =
    Decode.map2 TableSizeData
        (field "count" Decode.int)
        (field "words" Decode.int)


tableCopiesDecoder : Decoder TableCopiesData
tableCopiesDecoder =
    Decode.map3 TableCopiesData
        (field "disc" (Decode.list Decode.string))
        (field "mem" (Decode.list Decode.string))
        (field "both" (Decode.list Decode.string))


ipsDecoder : Decoder (List String)
ipsDecoder =
    Decode.list Decode.string


perfDataDecoder : Decoder PerfData
perfDataDecoder =
    Decode.map2 PerfData
        (field "cpu" cpusDataDecoder)
        (field "mem" memDataDecoder)


cpusDataDecoder : Decoder (List CpuData)
cpusDataDecoder =
    Decode.list cpuDataDecoder


cpuDataDecoder : Decoder CpuData
cpuDataDecoder =
    Decode.map3 CpuData
        (field "num" Decode.int)
        (field "busy" Decode.float)
        (field "idle" Decode.float)


memDataDecoder : Decoder MemData
memDataDecoder =
    Decode.map2 MemData
        (field "free_memory" Decode.int)
        (field "total_memory" Decode.int)


hostDecoder : Decoder HostData
hostDecoder =
    Decode.map HostData
        (field "host" Decode.string)


eventsDecoder : Decoder (List EventData)
eventsDecoder =
    Decode.list eventDecoder


eventDecoder : Decoder EventData
eventDecoder =
    Decode.map4 EventData
        (field "id" Decode.string)
        (field "date" Decode.int)
        (field "type" Decode.string)
        (field "info" Decode.string)


backupsDecoder : Decoder (List BackupData)
backupsDecoder =
    Decode.list backupDecoder


backupDecoder : Decoder BackupData
backupDecoder =
    Decode.map3 BackupData
        (field "id" Decode.string)
        (field "date" Decode.int)
        (field "info" Decode.string)
