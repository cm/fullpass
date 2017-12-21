module Encoders exposing (..)

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
            [ ( "action", Encode.string "cluster_nodes" )
            , ( "session", Encode.string session )
            ]
        )


encodeDeleteTableReplica : String -> String -> String -> String
encodeDeleteTableReplica session hostname tablename =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "cluster_delete_table" )
            , ( "session", Encode.string session )
            , ( "host", Encode.string hostname )
            , ( "table", Encode.string tablename )
            ]
        )


encodeAddReplica : String -> String -> String -> String -> String -> String
encodeAddReplica session hostname tablename peer media =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "cluster_add_table" )
            , ( "session", Encode.string session )
            , ( "host", Encode.string hostname )
            , ( "table", Encode.string tablename )
            , ( "peer", Encode.string peer )
            , ( "media", Encode.string media )
            ]
        )
