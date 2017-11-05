module Messages exposing (..)

import FileReader exposing (NativeFile)
import Http exposing (Error)
import Material
import Models exposing (..)
import Time exposing (..)
import WebRTC


type Msg
    = NoOp
    | WsMsg String
    | WsError String
    | WsAck String
    | WsPing Time
    | WsPong
    | ConnectOk String
    | ShowLoginPage
    | ShowRegisterPage
    | ShowSearchPage
    | ShowGroupsPage
    | ShowPeoplePage
    | ShowTalkPage
    | UsernameChanged String
    | PasswordChanged String
    | RegisterEmailChanged String
    | RegisterFirstChanged String
    | RegisterLastChanged String
    | RegisterPasswordChanged String
    | SearchKeywordsChanged String
    | Login
    | LoginOk ProfileData
    | LoginErr ErrorData
    | Register
    | RegisterErr ErrorData
    | Logout
    | LogoutOk
    | LogoutErr ErrorData
    | ProfileOk ProfileData
    | SearchGroup
    | CreateGroup
    | SelectGroup GroupData
    | GroupOk GroupData
    | GroupErr ErrorData
    | ParticipantOk ParticipantData
    | PresenceOk PresenceData
    | SearchPeople
    | PeopleOk ProfileData
    | SearchPeopleErr ErrorData
    | Invite GroupData ProfileData
    | AcceptInvite String
    | DeclineInvite String
    | AckInvite String
    | InviteOk InviteData
    | InviteErr ErrorData
    | JoinOk JoinData
    | JoinErr ErrorData
    | Increase
    | Reset
    | Mdl (Material.Msg Msg)
    | SelectTab Int
    | Capabilities
    | CapabilitiesOk WebRTC.CapabilitiesInfo
    | CapabilitiesErr String
    | StartVideoCall
    | TerminateVideoCall
    | PeerConnOk WebRTC.MediaInfo
    | PeerConnErr String
    | CallTerminatedOk String
    | CallTerminatedErr String
    | Ice String
    | LocalVideoOk String
    | LocalVideoErr String
    | RemoteOffer OfferData
    | RemoteIce IceData
    | AcceptCall
    | DeclineCall
    | AnswerOk AnswerData
    | AnswerMediaOk WebRTC.MediaInfo
    | AnswerMediaErr String
    | IceAddedOk String
    | IceAddedErr String
    | SdpAddedOk String
    | SdpAddedErr String
    | CompleteOk RoomData
    | CompleteErr ErrorData
    | FilesSelect (List NativeFile)
    | Upload
    | Uploaded (Result Http.Error String)
