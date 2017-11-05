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


encodeSignIn : LoginData -> String -> String
encodeSignIn data session =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "login" )
            , ( "username", Encode.string data.username )
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


encodeRegister : RegisterData -> String -> String
encodeRegister data session =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "register" )
            , ( "username", Encode.string data.email )
            , ( "password", Encode.string data.password )
            , ( "first", Encode.string data.first )
            , ( "last", Encode.string data.last )
            , ( "session", Encode.string session )
            ]
        )


encodeFetchInvites : String -> String
encodeFetchInvites session =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "invites" )
            , ( "session", Encode.string session )
            ]
        )


encodeFetchContacts : String -> String
encodeFetchContacts session =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "contacts" )
            , ( "session", Encode.string session )
            ]
        )


encodeFetchGroups : String -> String
encodeFetchGroups session =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "groups" )
            , ( "session", Encode.string session )
            ]
        )


encodeFetchParticipants : String -> String -> String
encodeFetchParticipants session gid =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "participants" )
            , ( "session", Encode.string session )
            , ( "group", Encode.string gid )
            ]
        )


encodeSearchGroup : String -> String -> String
encodeSearchGroup session name =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "group_search" )
            , ( "name", Encode.string name )
            , ( "session", Encode.string session )
            ]
        )


encodeCreateGroup : String -> String -> String
encodeCreateGroup session name =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "group_create" )
            , ( "name", Encode.string name )
            , ( "session", Encode.string session )
            ]
        )


encodeSearchPeople : String -> SearchData -> String
encodeSearchPeople session data =
    data.keywords
        |> encodeSearch session "people"


encodeInvite : String -> String -> String -> String
encodeInvite session gid uid =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "invite" )
            , ( "session", Encode.string session )
            , ( "group", Encode.string gid )
            , ( "user", Encode.string uid )
            ]
        )


encodeAcceptInvite : String -> String -> String
encodeAcceptInvite session id =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "invite_accept" )
            , ( "session", Encode.string session )
            , ( "invite", Encode.string id )
            ]
        )


encodeDeclineInvite : String -> String -> String
encodeDeclineInvite session id =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "invite_decline" )
            , ( "session", Encode.string session )
            , ( "invite", Encode.string id )
            ]
        )


encodeSearch : String -> String -> String -> String
encodeSearch session t k =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "search" )
            , ( "q", Encode.string k )
            , ( "type", Encode.string t )
            , ( "session", Encode.string session )
            ]
        )


encodeCapabilities : String -> String -> String -> String
encodeCapabilities session group sdp =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "rtc_capabilities" )
            , ( "group", Encode.string group )
            , ( "sdp", Encode.string sdp )
            , ( "session", Encode.string session )
            ]
        )


encodeAnswer : String -> String -> String -> String
encodeAnswer sdp group session =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "rtc_answer" )
            , ( "group", Encode.string group )
            , ( "sdp", Encode.string sdp )
            , ( "session", Encode.string session )
            ]
        )


encodeJoin : String -> String -> String
encodeJoin room session =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "join" )
            , ( "room", Encode.string room )
            , ( "session", Encode.string session )
            ]
        )


encodeOffer : String -> String -> String -> String
encodeOffer sdp room session =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "offer" )
            , ( "sdp", Encode.string sdp )
            , ( "room", Encode.string room )
            , ( "session", Encode.string session )
            ]
        )


encodeComplete : String -> String -> String
encodeComplete room session =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "complete" )
            , ( "room", Encode.string room )
            , ( "session", Encode.string session )
            ]
        )


encodeIce : String -> String -> String -> String
encodeIce ice room session =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "ice" )
            , ( "ice", Encode.string ice )
            , ( "room", Encode.string room )
            , ( "session", Encode.string session )
            ]
        )


encodeFile : NativeFile -> Http.Body
encodeFile f =
    multipartBody
        [ filePart "file" f
        ]
