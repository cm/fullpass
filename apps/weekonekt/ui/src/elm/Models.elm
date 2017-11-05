module Models exposing (..)

import Dict exposing (..)
import FileReader exposing (..)
import Material


type alias Flags =
    { api : String, ws : String, files : String }


type State
    = Connecting
    | SignedOut
    | SigningIn
    | Registering
    | LoggingOut
    | NewUser
    | Groups
    | People
    | Talk
    | Search
    | Searching
    | Joining
    | Created
    | Joined
    | Error
    | StartingVideoCall
    | StartedLocalPeerConn
    | TerminatingVideoCall
    | IncomingCall
    | InCall
    | CallStarting


type alias LoginData =
    { username : String
    , password : String
    , message : Maybe String
    }


type alias ProfileData =
    { username : String
    , first : String
    , last : String
    , id : String
    }


type alias ParticipantData =
    { user : ProfileData
    , group : String
    }


type alias PresenceData =
    { user : String
    , group : String
    , application : String
    , status : String
    }


type alias RegisterData =
    { email : String
    , first : String
    , last : String
    , password : String
    , message : Maybe String
    }


type alias GroupData =
    { id : String
    , name : String
    , owner : String
    }


type GroupPanel
    = GroupInfo
    | GroupMembers
    | GroupConference


type alias UserView =
    { user : Maybe ProfileData
    , presence : Dict String PresenceData
    }


type alias GroupView =
    { group : GroupData
    , invite : Maybe InviteData
    , participants : Dict String UserView
    }


type alias SearchData =
    { keywords : String
    , message : Maybe String
    }


type alias InviteData =
    { from : ProfileData
    , to : ProfileData
    , status : String
    , id : String
    , group : GroupData
    }


type alias MediaInfo =
    { sdp : Maybe String
    , url : Maybe String
    , ice : List String
    }


type alias ErrorData =
    { reason : String }


type alias RoomData =
    { status : String
    , name : String
    , participants : Int
    }


type alias OfferData =
    { room : String
    , sdp : String
    }


type alias JoinData =
    { room : String
    , sdp : String
    , status : String
    }


type alias AnswerData =
    { room : String
    , sdp : String
    }


type alias IceData =
    { room : String
    , ice : String
    }


type alias ConnData =
    { id : String }


type alias Model =
    { flags : Flags
    , loginData : Maybe LoginData
    , registerData : Maybe RegisterData
    , searchData : Maybe SearchData
    , session : String
    , count : Int
    , mdl : Material.Model
    , selectedTab : Int
    , room : Maybe RoomData
    , localMedia : Maybe MediaInfo
    , remoteMedia : Maybe MediaInfo
    , state : State
    , error : Maybe String
    , profile : Maybe ProfileData
    , people : List ProfileData
    , invitesSent : List InviteData
    , invitesReceived : List InviteData
    , groups : Dict String GroupView
    , group : Maybe GroupData
    , files : List NativeFile
    }


type alias Mdl =
    Material.Model
