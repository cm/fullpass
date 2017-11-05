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
        "join" ->
            Decode.map JoinErr
                (field "data" errorDecoder)

        "complete" ->
            Decode.map CompleteErr
                (field "data" errorDecoder)

        "login" ->
            Decode.map LoginErr
                (field "data" errorDecoder)

        "logout" ->
            Decode.map LogoutErr
                (field "data" errorDecoder)

        "register" ->
            Decode.map RegisterErr
                (field "data" errorDecoder)

        "group_create" ->
            Decode.map GroupErr
                (field "data" errorDecoder)

        "invite" ->
            Decode.map InviteErr
                (field "data" errorDecoder)

        _ ->
            ("Unsupported action: " ++ action ++ " when decoding error")
                |> Decode.fail


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
                (field "data" profileDecoder)

        "logout" ->
            Decode.succeed LogoutOk

        "profile" ->
            Decode.map ProfileOk
                (field "data" profileDecoder)

        "group" ->
            Decode.map GroupOk
                (field "data" groupDecoder)

        "people" ->
            Decode.map PeopleOk
                (field "data" profileDecoder)

        "invite" ->
            Decode.map InviteOk
                (field "data" inviteDecoder)

        "participant" ->
            Decode.map ParticipantOk
                (field "data" participantDecoder)

        "presence" ->
            Decode.map PresenceOk
                (field "data" presenceDecoder)

        "join" ->
            Decode.map JoinOk
                (field "data" joinDecoder)

        "answer" ->
            Decode.map AnswerOk
                (field "data" answerDecoder)

        "offer" ->
            Decode.map RemoteOffer
                (field "data" offerDecoder)

        "ice" ->
            Decode.map RemoteIce
                (field "data" iceDecoder)

        "complete" ->
            Decode.map CompleteOk
                (field "data" roomDecoder)

        "ping" ->
            Decode.succeed WsPong

        _ ->
            ("Unsupported action: " ++ action ++ " when decoding ok")
                |> Decode.fail


connectDecoder : Decoder String
connectDecoder =
    field "session" Decode.string


profileDecoder : Decoder ProfileData
profileDecoder =
    Decode.map4 ProfileData
        (field "username" Decode.string)
        (field "first" Decode.string)
        (field "last" Decode.string)
        (field "id" Decode.string)


groupDecoder : Decoder GroupData
groupDecoder =
    Decode.map3 GroupData
        (field "id" Decode.string)
        (field "name" Decode.string)
        (field "owner" Decode.string)


inviteDecoder : Decoder InviteData
inviteDecoder =
    Decode.map5 InviteData
        (field "from" profileDecoder)
        (field "to" profileDecoder)
        (field "status" Decode.string)
        (field "id" Decode.string)
        (field "group" groupDecoder)


contactsDecoder : Decoder (List ProfileData)
contactsDecoder =
    Decode.list profileDecoder


participantDecoder : Decoder ParticipantData
participantDecoder =
    Decode.map2 ParticipantData
        (field "user" profileDecoder)
        (field "group" Decode.string)


presenceDecoder : Decoder PresenceData
presenceDecoder =
    Decode.map4 PresenceData
        (field "user" Decode.string)
        (field "group" Decode.string)
        (field "application" Decode.string)
        (field "status" Decode.string)


joinDecoder : Decoder JoinData
joinDecoder =
    Decode.map3 JoinData
        (field "group" Decode.string)
        (field "sdp" Decode.string)
        (field "status" Decode.string)


roomDecoder : Decoder RoomData
roomDecoder =
    Decode.map3 RoomData
        (field "status" Decode.string)
        (field "name" Decode.string)
        (field "participants" Decode.int)


errorDecoder : Decoder ErrorData
errorDecoder =
    Decode.map ErrorData
        (field "reason" Decode.string)


offerDecoder : Decoder OfferData
offerDecoder =
    Decode.map2 OfferData
        (field "room" Decode.string)
        (field "sdp" Decode.string)


answerDecoder : Decoder AnswerData
answerDecoder =
    Decode.map2 AnswerData
        (field "room" Decode.string)
        (field "sdp" Decode.string)


iceDecoder : Decoder IceData
iceDecoder =
    Decode.map2 IceData
        (field "room" Decode.string)
        (field "ice" Decode.string)


etagDecoder : Decoder String
etagDecoder =
    field "ETag" string
