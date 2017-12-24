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


encodeAddReplica : String -> String -> String -> String -> String -> String
encodeAddReplica session hostname tablename peer media =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "add_table_replica" )
            , ( "session", Encode.string session )
            , ( "host", Encode.string hostname )
            , ( "table", Encode.string tablename )
            , ( "peer", Encode.string peer )
            , ( "media", Encode.string media )
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


encodeNewTableReplicas : TableMedia -> Dict String TableReplicaData -> Encode.Value
encodeNewTableReplicas media replicas =
    let
        nodes =
            replicas
                |> Dict.values
                |> List.foldl
                    (\r ->
                        \n ->
                            case r.media == media of
                                True ->
                                    r.node :: n

                                False ->
                                    n
                    )
                    []
    in
    nodes |> List.map Encode.string |> Encode.list


encodeCreateTable : String -> NewTableData -> String
encodeCreateTable session data =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "create_table" )
            , ( "session", Encode.string session )
            , ( "name", Encode.string data.name )
            , ( "type", data.storage |> encodeTableStorage |> Encode.string )
            , ( "memory", encodeNewTableReplicas Memory data.replicas )
            , ( "disc", encodeNewTableReplicas Disc data.replicas )
            , ( "both", encodeNewTableReplicas Both data.replicas )
            ]
        )
