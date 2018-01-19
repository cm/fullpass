module Encoders exposing (..)

import Dict exposing (..)
import FileReader exposing (..)
import Http exposing (..)
import Json.Encode as Encode exposing (encode, string)
import Models exposing (..)


encodePing : String
encodePing =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "ping" )
            ]
        )


encodeLogin : LoginData -> String -> String
encodeLogin data session =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "login" )
            , ( "username", Encode.string data.email )
            , ( "password", Encode.string data.password )
            , ( "session", Encode.string session )
            ]
        )


encodeLogout : String -> String
encodeLogout session =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "logout" )
            , ( "session", Encode.string session )
            ]
        )


encodeNodes : String -> String
encodeNodes session =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "nodes" )
            , ( "session", Encode.string session )
            ]
        )


encodeFetchTables : String -> String
encodeFetchTables session =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "cluster_tables" )
            , ( "session", Encode.string session )
            ]
        )


encodeDeleteTableReplica : String -> String -> String -> String
encodeDeleteTableReplica session hostname tablename =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "delete_table_replica" )
            , ( "session", Encode.string session )
            , ( "host", Encode.string hostname )
            , ( "table", Encode.string tablename )
            ]
        )


encodeDeleteSchema : String -> String -> String
encodeDeleteSchema session hostname =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "delete_schema" )
            , ( "session", Encode.string session )
            , ( "host", Encode.string hostname )
            ]
        )


encodeCreateSchema : String -> String -> String
encodeCreateSchema session hostname =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "create_schema" )
            , ( "session", Encode.string session )
            , ( "host", Encode.string hostname )
            ]
        )


encodeCreateTableReplica : String -> String -> String -> String -> String -> String
encodeCreateTableReplica session hostname tablename peer media =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "create_table_replica" )
            , ( "session", Encode.string session )
            , ( "host", Encode.string hostname )
            , ( "table", Encode.string tablename )
            , ( "peer", Encode.string peer )
            , ( "media", Encode.string media )
            ]
        )


encodeCreateSchemaReplica : String -> String -> String -> String
encodeCreateSchemaReplica session hostname peer =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "create_schema_replica" )
            , ( "session", Encode.string session )
            , ( "host", Encode.string hostname )
            , ( "peer", Encode.string peer )
            ]
        )


encodeTableStorage : TableStorage -> String
encodeTableStorage s =
    case s of
        Set ->
            "set"

        Bag ->
            "bag"

        OrderedSet ->
            "ordered_set"


encodeNewTableReplicas : TableMedia -> TableMedia -> Dict String TableReplicaData -> Encode.Value
encodeNewTableReplicas media definedMedia replicas =
    case media == definedMedia of
        False ->
            Encode.list []

        True ->
            replicas
                |> Dict.values
                |> List.map (\r -> r.node)
                |> List.map Encode.string
                |> Encode.list


encodeCreateTable : String -> NewTableData -> String
encodeCreateTable session data =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "create_table" )
            , ( "session", Encode.string session )
            , ( "name", Encode.string data.name )
            , ( "type", data.storage |> encodeTableStorage |> Encode.string )
            , ( "memory", encodeNewTableReplicas Memory data.media data.replicas )
            , ( "disc", encodeNewTableReplicas Disc data.media data.replicas )
            , ( "both", encodeNewTableReplicas Both data.media data.replicas )
            ]
        )


encodeCreateTableEverywhere : String -> TableData -> String
encodeCreateTableEverywhere session data =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "create_table_fully" )
            , ( "session", Encode.string session )
            , ( "name", Encode.string data.info.name )
            , ( "type", data.info.kind |> Encode.string )
            , ( "media", data.info.media |> Encode.string )
            ]
        )


encodeStartDb : String -> String -> String
encodeStartDb session host =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "start_db" )
            , ( "host", Encode.string host )
            , ( "session", Encode.string session )
            ]
        )


encodeStopDb : String -> String -> String
encodeStopDb session host =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "stop_db" )
            , ( "host", Encode.string host )
            , ( "session", Encode.string session )
            ]
        )


encodeFetchEvents : String -> String
encodeFetchEvents session =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "cluster_events" )
            , ( "session", Encode.string session )
            ]
        )


encodeClearEvents : String -> String
encodeClearEvents session =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "cluster_clear_events" )
            , ( "session", Encode.string session )
            ]
        )


encodeFetchBackups : String -> String
encodeFetchBackups session =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "backups" )
            , ( "session", Encode.string session )
            ]
        )


encodeCreateBackup : String -> NewBackupData -> String
encodeCreateBackup session data =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "create_backup" )
            , ( "name", Encode.string data.name )
            , ( "session", Encode.string session )
            ]
        )
