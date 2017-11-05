module Update exposing (..)

import Commands exposing (..)
import Decoders exposing (..)
import Init exposing (..)
import Material
import Messages exposing (..)
import Models exposing (..)
import Task exposing (Task)
import WebRTC exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        WsMsg str ->
            case wsResult str of
                Ok msg2 ->
                    update msg2 model

                Err e ->
                    update (WsError (toString e)) model

        WsPing _ ->
            model ! [ ping model.flags ]

        WsError e ->
            ( { model | error = Just e, state = Error }, Cmd.none )

        WsPong ->
            model ! []

        ConnectOk sid ->
            let
                model2 =
                    newModel model.flags
            in
            { model2 | session = sid, state = SignedOut, loginData = Just newLoginData } ! []

        FilesSelect files ->
            { model | files = files } ! []

        Upload ->
            model ! [ uploadFiles model.flags model.files model.session ]

        Uploaded (Ok _) ->
            model ! []

        Uploaded (Err e) ->
            model |> error (toString e)

        ShowRegisterPage ->
            { model | state = NewUser, registerData = Just newRegisterData } ! []

        ShowLoginPage ->
            { model | state = SignedOut } ! []

        ShowGroupsPage ->
            { model | state = Groups } ! []

        ShowPeoplePage ->
            { model | state = People } ! []

        ShowTalkPage ->
            { model | state = Talk } ! []

        ShowSearchPage ->
            { model | state = Search, searchData = Just newSearchData } ! []

        UsernameChanged v ->
            { model | loginData = Just (loginDataWithUsername v model.loginData) } ! []

        PasswordChanged v ->
            { model | loginData = Just (loginDataWithPassword v model.loginData) } ! []

        RegisterFirstChanged v ->
            { model | registerData = Just (registerDataWithFirst v model.registerData) } ! []

        RegisterLastChanged v ->
            { model | registerData = Just (registerDataWithLast v model.registerData) } ! []

        RegisterEmailChanged v ->
            { model | registerData = Just (registerDataWithEmail v model.registerData) } ! []

        RegisterPasswordChanged v ->
            { model | registerData = Just (registerDataWithPassword v model.registerData) } ! []

        SearchKeywordsChanged v ->
            { model | searchData = Just (searchDataWithKeywords v model.searchData) } ! []

        Login ->
            case model.loginData of
                Nothing ->
                    model |> error "No login data"

                Just d ->
                    { model | state = SigningIn } ! [ signIn model.flags d model.session ]

        LoginOk p ->
            { model | profile = Just p, state = Groups } ! []

        LoginErr r ->
            { model | loginData = Just (loginDataWithMessage r.reason model.loginData), state = SignedOut } ! []

        Register ->
            case model.registerData of
                Nothing ->
                    model |> error "No register data"

                Just d ->
                    { model | state = Registering } ! [ register model.flags d model.session ]

        RegisterErr r ->
            { model
                | registerData = Just (registerDataWithMessage r.reason model.registerData)
                , state = NewUser
            }
                ! []

        Logout ->
            { model
                | state = LoggingOut
                , loginData = Just newLoginData
                , registerData = Just newRegisterData
            }
                ! [ logout model.flags model.session ]

        LogoutOk ->
            newModel model.flags
                |> update (ConnectOk model.session)

        LogoutErr r ->
            model |> error r.reason

        SearchGroup ->
            case model.searchData of
                Nothing ->
                    model |> error "No search data"

                Just d ->
                    model
                        ! [ d.keywords |> searchGroup model.flags model.session ]

        SelectGroup data ->
            { model | group = Just data } ! []

        GroupOk data ->
            { model | groups = groupsWithGroup data model.groups }
                ! [ fetchParticipants model.flags model.session data.id
                  ]

        GroupErr e ->
            model |> error e.reason

        ParticipantOk data ->
            { model | groups = groupsWithParticipant data model.groups }
                ! []

        PresenceOk data ->
            { model | groups = groupsWithPresence data model.groups }
                ! []

        CreateGroup ->
            case model.searchData of
                Nothing ->
                    model |> error "No group name"

                Just d ->
                    model
                        ! [ d.keywords |> createGroup model.flags model.session ]

        PeopleOk p ->
            { model | people = sortedPeople p model.people } ! []

        SearchPeopleErr e ->
            { model
                | searchData = Just (searchDataWithMessage e.reason model.searchData)
            }
                ! []

        SearchPeople ->
            case model.searchData of
                Nothing ->
                    model |> error "No register data"

                Just d ->
                    { model | people = [] }
                        ! [ d |> searchPeople model.flags model.session ]

        ProfileOk p ->
            case model.state of
                Registering ->
                    { model | profile = Just p, state = Groups } ! []

                SigningIn ->
                    { model | profile = Just p, state = Groups }
                        ! [ fetchInvites model.flags model.session
                          , fetchGroups model.flags model.session
                          ]

                _ ->
                    { model | profile = Just p } ! []

        Invite g u ->
            model ! [ invite model.flags model.session g.id u.id ]

        InviteErr e ->
            case e.reason of
                "conflict" ->
                    model |> error "This person has already been invited"

                r ->
                    model |> error r

        InviteOk i ->
            case model.profile of
                Nothing ->
                    error "No profile data" model

                Just p ->
                    case i.from.id == p.id of
                        True ->
                            { model | invitesSent = mergedInvites i model.invitesSent } ! []

                        False ->
                            { model | groups = groupsWithInvite i model.groups } ! []

        AcceptInvite i ->
            model ! [ i |> acceptInvite model.flags model.session ]

        DeclineInvite i ->
            model ! [ i |> declineInvite model.flags model.session ]

        AckInvite i ->
            model ! []

        JoinOk data ->
            model ! [ WebRTC.answer data.sdp [] AnswerMediaOk AnswerMediaErr ]

        WsAck _ ->
            ( model, Cmd.none )

        JoinErr e ->
            ( { model | error = Just e.reason, state = Error }, Cmd.none )

        Increase ->
            ( { model | count = model.count + 1 }
            , Cmd.none
            )

        Reset ->
            ( { model | count = 0 }
            , Cmd.none
            )

        SelectTab num ->
            { model | selectedTab = num } ! []

        Mdl msg2 ->
            Material.update Mdl msg2 model

        Capabilities ->
            model
                ! [ WebRTC.capabilities CapabilitiesOk CapabilitiesErr
                  ]

        CapabilitiesOk caps ->
            model ! [ sendCapabilities model.flags model.session "dummy" caps.sdp ]

        CapabilitiesErr e ->
            error e model

        StartVideoCall ->
            { model | state = StartingVideoCall }
                ! [ WebRTC.call PeerConnOk PeerConnErr
                  ]

        TerminateVideoCall ->
            { model | state = TerminatingVideoCall }
                ! [ WebRTC.hangup CallTerminatedOk CallTerminatedErr
                  ]

        PeerConnOk m ->
            case model.room of
                Nothing ->
                    error "No room available" model

                Just r ->
                    { model | state = StartedLocalPeerConn, localMedia = Just (media m) }
                        ! [ sendOffer model.flags m.sdp r.name model.session ]

        PeerConnErr e ->
            error e model

        CallTerminatedOk _ ->
            { model | state = Groups, localMedia = Nothing }
                ! []

        CallTerminatedErr e ->
            error e model

        LocalVideoOk id ->
            ( model, Cmd.none )

        LocalVideoErr reason ->
            ( { model | error = Just reason, state = Error }, Cmd.none )

        Ice ice ->
            case model.room of
                Nothing ->
                    error "No room available" model

                Just r ->
                    case model.localMedia of
                        Nothing ->
                            { model | localMedia = Just (newMediaWithIce ice) }
                                ! [ sendIce model.flags ice r.name model.session ]

                        Just media ->
                            { model | localMedia = Just (mediaWithNewIce ice media) }
                                ! [ sendIce model.flags ice r.name model.session ]

        RemoteOffer o ->
            case model.remoteMedia of
                Nothing ->
                    { model
                        | state = IncomingCall
                        , remoteMedia = Just (newMediaWithSdp o.sdp)
                    }
                        ! []

                --    ! [ WebRTC.answerSdp o.sdp AnswerMediaOk AnswerMediaErr ]
                Just m ->
                    error "There is already a remote media" model

        --( { model
        --    | state = IncomingCall
        --    , remoteMedia = Just (mediaWithNewSdp o.sdp m)
        --  }
        --, WebRTC.answerSdp o.sdp AnswerOk AnswerErr
        -- )
        RemoteIce i ->
            case model.localMedia of
                Nothing ->
                    case model.remoteMedia of
                        Nothing ->
                            { model | remoteMedia = Just (newMediaWithIce i.ice) } ! []

                        Just m ->
                            { model | remoteMedia = Just (mediaWithNewIce i.ice m) } ! []

                Just m ->
                    model ! [ WebRTC.addIce i.ice IceAddedOk IceAddedErr ]

        --case model.localMedia of
        --    Nothing ->
        --        model
        --            ! [ WebRTC.answerIce i.ice AnswerOk AnswerErr
        --              ]
        --    Just m ->
        --        model ! [ WebRTC.addIce i.ice IceAddedOk IceAddedErr ]
        DeclineCall ->
            ( { model | state = Joined, remoteMedia = Nothing }, Cmd.none )

        AcceptCall ->
            case model.remoteMedia of
                Nothing ->
                    error "No remote media available" model

                Just m ->
                    case m.sdp of
                        Nothing ->
                            error "No sdp in remote media available" model

                        Just sdp ->
                            { model | state = CallStarting }
                                ! [ WebRTC.answer sdp m.ice AnswerMediaOk AnswerMediaErr ]

        AnswerMediaOk m ->
            model ! [ sendAnswer model.flags m.sdp "dummy" model.session ]

        --case model.room of
        --    Nothing ->
        --        error "No room available" model
        --    Just r ->
        --        { model | state = CallStarting, localMedia = Just (media m) }
        --            ! [ sendAnswer model.flags m.sdp r.name model.session ]
        AnswerMediaErr r ->
            error r model

        AnswerOk a ->
            case model.room of
                Nothing ->
                    error "No room available" model

                Just r ->
                    case model.localMedia of
                        Nothing ->
                            model |> error "No call was initiated"

                        Just _ ->
                            model ! [ WebRTC.addSdp a.sdp SdpAddedOk SdpAddedErr ]

        SdpAddedOk sdp ->
            case model.room of
                Nothing ->
                    model |> error "No room available"

                Just r ->
                    let
                        m =
                            case model.remoteMedia of
                                Nothing ->
                                    newMediaWithSdp sdp

                                Just m ->
                                    mediaWithNewSdp sdp m
                    in
                    { model | remoteMedia = Just m, state = InCall } ! [ sendComplete model.flags r.name model.session ]

        SdpAddedErr r ->
            model |> error r

        IceAddedOk i ->
            case model.remoteMedia of
                Nothing ->
                    { model
                        | remoteMedia = Just (newMediaWithIce i)
                    }
                        ! []

                Just m ->
                    { model
                        | remoteMedia = Just (mediaWithNewIce i m)
                    }
                        ! []

        IceAddedErr r ->
            error r model

        CompleteOk r ->
            { model | state = InCall } ! []

        CompleteErr e ->
            model |> error e.reason


error : String -> Model -> ( Model, Cmd msg )
error e model =
    ( { model | error = Just e, state = Error }, Cmd.none )
