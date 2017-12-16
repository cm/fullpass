effect module WebRTC
    where { subscription = MySub, command = MyCmd }
    exposing
        ( CapabilitiesInfo
        , MediaInfo
        , addIce
        , addSdp
        , answer
        , call
        , capabilities
        , hangup
        , ice
        )

import Debug
import Dict exposing (Dict)
import Native.WebRTC
import Process
import Task exposing (Task)
import Time exposing (..)


capabilities : (CapabilitiesInfo -> msg) -> (String -> msg) -> Cmd msg
capabilities ok err =
    command (Capabilities ok err)


call : (MediaInfo -> msg) -> (String -> msg) -> Cmd msg
call ok err =
    command (Call ok err)


hangup : (String -> msg) -> (String -> msg) -> Cmd msg
hangup ok err =
    command (Hangup ok err)


answer : String -> List String -> (MediaInfo -> msg) -> (String -> msg) -> Cmd msg
answer sdp ice ok err =
    command (Answer sdp ice ok err)


addIce : String -> (String -> msg) -> (String -> msg) -> Cmd msg
addIce ice ok err =
    command (AddIce ice ok err)


addSdp : String -> (String -> msg) -> (String -> msg) -> Cmd msg
addSdp sdp ok err =
    command (AddSdp sdp ok err)


type MySub msg
    = Ice (String -> msg)


type MyCmd msg
    = Call (MediaInfo -> msg) (String -> msg)
    | Capabilities (CapabilitiesInfo -> msg) (String -> msg)
    | Hangup (String -> msg) (String -> msg)
    | Answer String (List String) (MediaInfo -> msg) (String -> msg)
    | AddSdp String (String -> msg) (String -> msg)
    | AddIce String (String -> msg) (String -> msg)


type alias State msg =
    { iceSubs : List (String -> msg)
    }


stateWithSubs : List (MySub msg) -> State msg -> State msg
stateWithSubs subs state =
    { iceSubs = extractIceSubs subs
    }


extractIceSubs : List (MySub msg) -> List (String -> msg)
extractIceSubs subs =
    List.map
        (\s ->
            case s of
                Ice tagger ->
                    tagger
        )
        subs


ice : (String -> msg) -> Sub msg
ice tagger =
    subscription (Ice tagger)


init : Task Never (State msg)
init =
    Task.succeed { iceSubs = [] }


subMap : (a -> b) -> MySub a -> MySub b
subMap f sub =
    case sub of
        Ice tagger ->
            Ice (tagger >> f)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap f cmd =
    case cmd of
        Call ok err ->
            Call (ok >> f) (err >> f)

        Capabilities ok err ->
            Capabilities (ok >> f) (err >> f)

        Hangup ok err ->
            Hangup (ok >> f) (err >> f)

        Answer sdp ice ok err ->
            Answer sdp ice (ok >> f) (err >> f)

        AddSdp sdp ok err ->
            AddSdp sdp (ok >> f) (err >> f)

        AddIce ice ok err ->
            AddIce ice (ok >> f) (err >> f)


onEffects : Platform.Router msg Msg -> List (MyCmd msg) -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router cmds subs state =
    case cmds of
        [] ->
            Task.succeed (stateWithSubs subs state)

        (Call ok err) :: cmds ->
            attemptCall router ok err
                &> Task.succeed (stateWithSubs subs state)

        (Capabilities ok err) :: cmds ->
            attemptCapabilities router ok err
                &> Task.succeed (stateWithSubs subs state)

        (Hangup ok err) :: cmds ->
            attemptHangup router ok err
                &> Task.succeed (stateWithSubs subs state)

        (Answer sdp ice ok err) :: cmds ->
            attemptAnswer router sdp ice ok err
                &> Task.succeed (stateWithSubs subs state)

        (AddSdp sdp ok err) :: cmds ->
            attemptAddSdp router sdp ok err
                &> Task.succeed (stateWithSubs subs state)

        (AddIce ice ok err) :: cmds ->
            attemptAddIce router ice ok err
                &> Task.succeed (stateWithSubs subs state)


attemptCapabilities : Platform.Router msg Msg -> (CapabilitiesInfo -> msg) -> (String -> msg) -> Task Never Process.Id
attemptCapabilities router ok err =
    Process.spawn <|
        (capabilitiesTask (callSettings router)
            |> Task.andThen
                (\caps ->
                    Platform.sendToApp router (ok caps)
                )
            |> Task.onError
                (\reason ->
                    Platform.sendToApp router (err reason)
                )
        )


attemptCall : Platform.Router msg Msg -> (MediaInfo -> msg) -> (String -> msg) -> Task Never Process.Id
attemptCall router ok err =
    Process.spawn <|
        (callTask (callSettings router)
            |> Task.andThen
                (\media ->
                    Platform.sendToApp router (ok media)
                )
            |> Task.onError
                (\reason ->
                    Platform.sendToApp router (err reason)
                )
        )


attemptHangup : Platform.Router msg Msg -> (String -> msg) -> (String -> msg) -> Task Never Process.Id
attemptHangup router ok err =
    Process.spawn <|
        (hangupTask (callSettings router)
            |> Task.andThen
                (\media ->
                    Platform.sendToApp router (ok media)
                )
            |> Task.onError
                (\reason ->
                    Platform.sendToApp router (err reason)
                )
        )


attemptAnswer : Platform.Router msg Msg -> String -> List String -> (MediaInfo -> msg) -> (String -> msg) -> Task Never Process.Id
attemptAnswer router sdp ice ok err =
    Process.spawn <|
        (answerTask sdp ice (callSettings router)
            |> Task.andThen
                (\media ->
                    Platform.sendToApp router (ok media)
                )
            |> Task.onError
                (\reason ->
                    Platform.sendToApp router (err reason)
                )
        )


attemptAddIce : Platform.Router msg Msg -> String -> (String -> msg) -> (String -> msg) -> Task Never Process.Id
attemptAddIce router ice ok err =
    Process.spawn <|
        (addIceTask ice
            |> Task.andThen
                (\media ->
                    Platform.sendToApp router (ok media)
                )
            |> Task.onError
                (\reason ->
                    Platform.sendToApp router (err reason)
                )
        )


attemptAddSdp : Platform.Router msg Msg -> String -> (String -> msg) -> (String -> msg) -> Task Never Process.Id
attemptAddSdp router sdp ok err =
    Process.spawn <|
        (addSdpTask sdp
            |> Task.andThen
                (\media ->
                    Platform.sendToApp router (ok media)
                )
            |> Task.onError
                (\reason ->
                    Platform.sendToApp router (err reason)
                )
        )


capabilitiesTask : Settings -> Task String CapabilitiesInfo
capabilitiesTask settings =
    Native.WebRTC.capabilities settings


callTask : Settings -> Task String MediaInfo
callTask settings =
    Native.WebRTC.call settings


hangupTask : Settings -> Task String String
hangupTask settings =
    Native.WebRTC.hangup settings


answerTask : String -> List String -> Settings -> Task String MediaInfo
answerTask sdp ice settings =
    Native.WebRTC.answer sdp ice settings


addSdpTask : String -> Task String String
addSdpTask sdp =
    Native.WebRTC.addSdp sdp


addIceTask : String -> Task String String
addIceTask ice =
    Native.WebRTC.addIce ice


type alias Settings =
    { onIce : String -> String -> Task Never () }


type alias MediaInfo =
    { sdp : String, url : String }


type alias CapabilitiesInfo =
    { sdp : String }


callSettings : Platform.Router msg Msg -> Settings
callSettings router =
    { onIce =
        \ice _ ->
            Platform.sendToSelf
                router
                (IceCandidate ice)
    }


(&>) t1 t2 =
    Task.andThen (\_ -> t2) t1


type Msg
    = IceCandidate String


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router msg state =
    case msg of
        IceCandidate c ->
            Task.sequence
                (state.iceSubs
                    |> List.map (\tagger -> Platform.sendToApp router (tagger c))
                )
                &> Task.succeed state
