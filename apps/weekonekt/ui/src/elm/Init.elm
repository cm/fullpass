module Init exposing (..)

import Commands exposing (..)
import Dict exposing (..)
import Material
import Messages exposing (..)
import Models exposing (..)
import WebRTC


newModel : Flags -> Model
newModel flags =
    { flags = flags
    , session = ""
    , count = 0
    , mdl = Material.model
    , selectedTab = 0
    , room = Nothing
    , localMedia = Nothing
    , remoteMedia = Nothing
    , state = Connecting
    , error = Nothing
    , loginData = Nothing
    , registerData = Nothing
    , searchData = Nothing
    , profile = Nothing
    , people = []
    , invitesSent = []
    , invitesReceived = []
    , groups = Dict.empty
    , group = Nothing
    , files = []
    }


stringOrEmpty : Maybe String -> String
stringOrEmpty str =
    case str of
        Nothing ->
            ""

        Just s ->
            s


newLoginData : LoginData
newLoginData =
    { username = ""
    , password = ""
    , message = Nothing
    }


newRegisterData : RegisterData
newRegisterData =
    { email = ""
    , password = ""
    , first = ""
    , last = ""
    , message = Nothing
    }


newSearchData : SearchData
newSearchData =
    { keywords = ""
    , message = Nothing
    }


loginDataOrNew : Maybe LoginData -> LoginData
loginDataOrNew data =
    case data of
        Nothing ->
            newLoginData

        Just d ->
            d


searchDataOrNew : Maybe SearchData -> SearchData
searchDataOrNew data =
    case data of
        Nothing ->
            newSearchData

        Just d ->
            d


loginDataWithPassword : String -> Maybe LoginData -> LoginData
loginDataWithPassword p data =
    let
        d =
            loginDataOrNew data
    in
    { d | password = p }


loginDataWithUsername : String -> Maybe LoginData -> LoginData
loginDataWithUsername u data =
    let
        d =
            loginDataOrNew data
    in
    { d | username = u }


loginDataWithMessage : String -> Maybe LoginData -> LoginData
loginDataWithMessage m data =
    let
        d =
            loginDataOrNew data
    in
    { d | message = Just m, password = "" }


registerDataOrNew : Maybe RegisterData -> RegisterData
registerDataOrNew data =
    case data of
        Nothing ->
            newRegisterData

        Just d ->
            d


registerDataWithPassword : String -> Maybe RegisterData -> RegisterData
registerDataWithPassword p data =
    let
        d =
            registerDataOrNew data
    in
    { d | password = p }


registerDataWithFirst : String -> Maybe RegisterData -> RegisterData
registerDataWithFirst f data =
    let
        d =
            registerDataOrNew data
    in
    { d | first = f }


registerDataWithLast : String -> Maybe RegisterData -> RegisterData
registerDataWithLast l data =
    let
        d =
            registerDataOrNew data
    in
    { d | last = l }


registerDataWithEmail : String -> Maybe RegisterData -> RegisterData
registerDataWithEmail e data =
    let
        d =
            registerDataOrNew data
    in
    { d | email = e }


registerDataWithMessage : String -> Maybe RegisterData -> RegisterData
registerDataWithMessage m data =
    let
        d =
            registerDataOrNew data
    in
    { d | message = Just m, password = "" }


searchDataWithMessage : String -> Maybe SearchData -> SearchData
searchDataWithMessage m data =
    let
        d =
            searchDataOrNew data
    in
    { d | message = Just m }


searchDataWithKeywords : String -> Maybe SearchData -> SearchData
searchDataWithKeywords k data =
    let
        d =
            searchDataOrNew data
    in
    { d | keywords = k }


usersWithUser : ProfileData -> Dict String UserView -> Dict String UserView
usersWithUser u users =
    case Dict.get u.id users of
        Nothing ->
            Dict.insert u.id (newUserView u) users

        Just existing ->
            Dict.insert u.id (userViewWithUser u existing) users


newGroupView : GroupData -> GroupView
newGroupView g =
    { group = g
    , invite = Nothing
    , participants = Dict.empty
    }


newUserView : ProfileData -> UserView
newUserView u =
    { user = Just u
    , presence = Dict.empty
    }


newUserViewWithPresence : PresenceData -> UserView
newUserViewWithPresence p =
    { user = Nothing
    , presence = Dict.insert p.application p Dict.empty
    }


groupViewWithGroup : GroupData -> GroupView -> GroupView
groupViewWithGroup g v =
    { group = g
    , invite = v.invite
    , participants = v.participants
    }


userViewWithUser : ProfileData -> UserView -> UserView
userViewWithUser u v =
    { user = Just u
    , presence = v.presence
    }


userViewWithPresence : PresenceData -> UserView -> UserView
userViewWithPresence p u =
    { user = u.user
    , presence = Dict.insert p.application p u.presence
    }


groupViewWithInvite : GroupData -> InviteData -> GroupView
groupViewWithInvite g i =
    { group = g
    , invite = Just i
    , participants = Dict.empty
    }


groupViewWithoutInvite : GroupView -> GroupView
groupViewWithoutInvite v =
    { group = v.group
    , invite = Nothing
    , participants = v.participants
    }


groupViewWithParticipant : ParticipantData -> GroupView -> GroupView
groupViewWithParticipant p g =
    { group = g.group
    , invite = g.invite
    , participants = usersWithUser p.user g.participants
    }


groupViewWithParticipants : Dict String UserView -> GroupView -> GroupView
groupViewWithParticipants p v =
    { group = v.group
    , invite = v.invite
    , participants = p
    }


groupViewWithPresence : PresenceData -> GroupView -> GroupView
groupViewWithPresence p g =
    case Dict.get p.user g.participants of
        Just u ->
            groupViewWithParticipants
                (Dict.insert p.user (userViewWithPresence p u) g.participants)
                g

        Nothing ->
            groupViewWithParticipants
                (Dict.insert p.user (newUserViewWithPresence p) g.participants)
                g


groupsWithGroup : GroupData -> Dict String GroupView -> Dict String GroupView
groupsWithGroup g groups =
    case Dict.get g.id groups of
        Just existing ->
            Dict.insert g.id (groupViewWithGroup g existing) groups

        Nothing ->
            Dict.insert g.id (newGroupView g) groups


groupsWithInvite : InviteData -> Dict String GroupView -> Dict String GroupView
groupsWithInvite i groups =
    case i.status of
        "new" ->
            Dict.insert i.group.id (groupViewWithInvite i.group i) groups

        "accepted" ->
            case Dict.get i.group.id groups of
                Nothing ->
                    Dict.insert i.group.id (newGroupView i.group) groups

                Just g ->
                    Dict.insert i.group.id (groupViewWithoutInvite g) groups

        _ ->
            Dict.remove i.group.id groups


groupsWithParticipant : ParticipantData -> Dict String GroupView -> Dict String GroupView
groupsWithParticipant p groups =
    case Dict.get p.group groups of
        Nothing ->
            groups

        Just g ->
            Dict.insert p.group (groupViewWithParticipant p g) groups


groupsWithPresence : PresenceData -> Dict String GroupView -> Dict String GroupView
groupsWithPresence p groups =
    case Dict.get p.group groups of
        Nothing ->
            groups

        Just g ->
            Dict.insert p.group (groupViewWithPresence p g) groups


sortedPeople : ProfileData -> List ProfileData -> List ProfileData
sortedPeople p others =
    -- TODO
    p :: others


isSameInvite : InviteData -> InviteData -> Bool
isSameInvite i1 i2 =
    i1.id == i2.id


addOrReplaceInvite : InviteData -> List InviteData -> Bool -> List InviteData -> List InviteData
addOrReplaceInvite invite rem wasAdded result =
    case rem of
        [] ->
            case wasAdded of
                True ->
                    result

                False ->
                    invite :: result

        i :: rest ->
            case isSameInvite i invite of
                False ->
                    addOrReplaceInvite invite rest wasAdded (i :: result)

                True ->
                    addOrReplaceInvite invite rest True (invite :: result)


mergedInvites : InviteData -> List InviteData -> List InviteData
mergedInvites invite invites =
    addOrReplaceInvite invite invites False []


isInviteInStatus : String -> InviteData -> Bool
isInviteInStatus s i =
    i.status == s


invitesInStatus : String -> List InviteData -> List InviteData
invitesInStatus s invites =
    invites
        |> List.filter (isInviteInStatus s)


isInvited : ProfileData -> List InviteData -> Bool
isInvited profile invites =
    case invites of
        [] ->
            False

        i :: rest ->
            case i.from.id == profile.id || i.to.id == profile.id of
                True ->
                    True

                False ->
                    isInvited profile rest


isContact : ProfileData -> List ProfileData -> Bool
isContact profile others =
    case others of
        [] ->
            False

        p :: rest ->
            case p.id == profile.id of
                True ->
                    True

                False ->
                    isContact profile rest


conn : String -> ConnData
conn id =
    { id = id }


init : Flags -> ( Model, Cmd Msg )
init flags =
    newModel flags ! []


media : WebRTC.MediaInfo -> MediaInfo
media m =
    { sdp = Just m.sdp
    , url = Just m.url
    , ice = []
    }


newMediaWithIce : String -> MediaInfo
newMediaWithIce ice =
    { sdp = Nothing
    , url = Nothing
    , ice = [ ice ]
    }


mediaWithNewIce : String -> MediaInfo -> MediaInfo
mediaWithNewIce ice m =
    { sdp = m.sdp
    , url = m.url
    , ice = ice :: m.ice
    }


newMediaWithSdp : String -> MediaInfo
newMediaWithSdp sdp =
    { sdp = Just sdp
    , url = Nothing
    , ice = []
    }


mediaWithNewSdp : String -> MediaInfo -> MediaInfo
mediaWithNewSdp sdp m =
    { sdp = Just sdp
    , url = m.url
    , ice = m.ice
    }
