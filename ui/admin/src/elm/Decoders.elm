module Decoders exposing (..)

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

        "cluster_nodes" ->
            Decode.map Nodes
                (field "data" nodesDecoder)

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
    Decode.map5 TableData
        (field "id" Decode.string)
        (field "name" Decode.string)
        (field "type" Decode.string)
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