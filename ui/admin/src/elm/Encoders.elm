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
