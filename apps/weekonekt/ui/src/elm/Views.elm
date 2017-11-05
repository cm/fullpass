module Views exposing (..)

import Dict exposing (..)
import FileReader exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Init exposing (..)
import Json.Decode as Json
import MDKit exposing (..)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Color as Color
import Material.Dialog as Dialog
import Material.Grid as Grid exposing (Device(..), cell, grid, size)
import Material.Layout as Layout
import Material.List as MdList
import Material.Options as Options exposing (css, styled)
import Material.Progress as Loading
import Material.Scheme
import Material.Textfield as Textfield
import Material.Typography as Typo
import Messages exposing (..)
import Models exposing (..)
import String.Extra exposing (..)
import UIKit exposing (..)


view : Model -> Html Msg
view model =
    case model.state of
        Error ->
            case model.error of
                Nothing ->
                    "Unknown error"
                        |> errorView model

                Just e ->
                    e |> errorView model

        Groups ->
            materialGroupsView model

        People ->
            peopleView model

        Talk ->
            talkView model

        Search ->
            case model.searchData of
                Nothing ->
                    "No search data" |> errorView model

                Just d ->
                    d
                        |> searchView model

        Searching ->
            case model.searchData of
                Nothing ->
                    "No search data" |> errorView model

                Just d ->
                    d
                        |> searchingView model

        Connecting ->
            connectingView model

        SignedOut ->
            case model.loginData of
                Nothing ->
                    "No login data" |> errorView model

                Just d ->
                    materialLoginView d model

        NewUser ->
            case model.registerData of
                Nothing ->
                    "No register data" |> errorView model

                Just d ->
                    materialRegisterView d model

        SigningIn ->
            signingInView model

        Registering ->
            registeringView model

        LoggingOut ->
            loggingOutView model

        Joining ->
            joiningView model

        Created ->
            case model.room of
                Nothing ->
                    "No room!" |> errorView model

                Just room ->
                    createdView room model

        Joined ->
            case model.room of
                Nothing ->
                    "No room!" |> errorView model

                Just room ->
                    roomView room model

        StartingVideoCall ->
            startingVideoView model

        TerminatingVideoCall ->
            terminatingVideoCallView model

        StartedLocalPeerConn ->
            case model.room of
                Nothing ->
                    "No room" |> errorView model

                Just r ->
                    case model.localMedia of
                        Nothing ->
                            "No local media" |> errorView model

                        Just media ->
                            outgoingCallView r media model

        IncomingCall ->
            case model.room of
                Nothing ->
                    "No room" |> errorView model

                Just r ->
                    case model.remoteMedia of
                        Nothing ->
                            "No remote media" |> errorView model

                        Just media ->
                            incomingCallView r media model

        CallStarting ->
            callStartingView model

        InCall ->
            case model.room of
                Nothing ->
                    "No room" |> errorView model

                Just r ->
                    case ( model.localMedia, model.remoteMedia ) of
                        ( Just local, Just remote ) ->
                            callView r local remote model

                        ( Nothing, _ ) ->
                            "No local media" |> errorView model

                        ( _, Nothing ) ->
                            "No remote media" |> errorView model


stdLayoutView : Model -> List (Html Msg) -> Html Msg
stdLayoutView model m =
    div [ class "wrapper" ]
        [ notificationsView model
        , div [ class "main" ]
            m
        ]


navLayoutView : Model -> Nav -> List (Html Msg) -> Html Msg
navLayoutView model nav contents =
    div []
        [ navView nav
        , contents
            |> stdLayoutView model
        ]


type alias Col =
    { span : Int
    , align : Align
    , contents : List (Html Msg)
    , background : String
    }


colsLayout : List Col -> Html Msg
colsLayout cols =
    div [ class "row", style [ ( "margin-bottom", "15px" ) ] ]
        (List.map
            (\col ->
                div
                    [ class
                        ("col-md-"
                            ++ toString col.span
                            ++ " col-xl-"
                            ++ toString col.span
                            ++ " text-"
                            ++ alignStyle col.align
                            ++ " bg-"
                            ++ col.background
                        )
                    ]
                    col.contents
            )
            cols
        )


colsLayoutView : Model -> List Col -> List (Html Msg)
colsLayoutView model cols =
    List.map
        (\col ->
            div
                [ class
                    ("col-md-"
                        ++ toString col.span
                        ++ " col-xl-"
                        ++ toString col.span
                        ++ " text-"
                        ++ alignStyle col.align
                        ++ " bg-"
                        ++ col.background
                    )
                ]
                col.contents
        )
        cols


rowsLayoutView : List (Html Msg) -> List (Html Msg)
rowsLayoutView rows =
    rows
        |> List.concatMap
            (\r ->
                [ r ]
            )


tabView : String -> List Link -> List (Html Msg) -> List (Html Msg)
tabView title header contents =
    div [ class "row" ]
        [ div [ class "col-md-12 text-center" ]
            [ h3 [] [ text title ]
            , div [] [ text "links here" ]
            ]
        ]
        :: contents


type alias Nav =
    { title : String
    , links : List Link
    }


navLinkView : Link -> Html Msg
navLinkView link =
    li [ class "nav-item" ]
        [ a [ class "nav-link", href "#", onClick link.action ]
            [ link.icon |> iconOrDefault |> nucleoIcon
            , span [ class "link-title" ]
                [ text link.title ]
            ]
        ]


navView : Nav -> Html Msg
navView data =
    nav [ class "navbar bg-primary fixed-top" ]
        [ div [ class "container" ]
            [ div [ class "float-left" ]
                [ a [ href "#", class "navbar-brand" ] [ text data.title ]
                ]
            , div
                [ class "float-right"
                ]
                [ ul [ class "navbar-nav" ]
                    (data.links
                        |> List.map navLinkView
                    )
                ]
            ]
        ]


type Align
    = Left
    | Center
    | Right


type alias Section =
    { align : Align
    , contents : List (Html Msg)
    , kind : String
    }


alignStyle : Align -> String
alignStyle k =
    case k of
        Left ->
            "left"

        Center ->
            "center"

        Right ->
            "right"


basicSection : Model -> List (Html Msg) -> List (Html Msg)
basicSection model contents =
    { kind = "dark"
    , align = Center
    , contents = contents
    }
        |> sectionView model


transparentSection : Model -> List (Html Msg) -> List (Html Msg)
transparentSection model contents =
    { kind = "transparent"
    , align = Center
    , contents = contents
    }
        |> sectionView model


graySection : Model -> List (Html Msg) -> List (Html Msg)
graySection model contents =
    { kind = "gray"
    , align = Center
    , contents = contents
    }
        |> sectionView model


sectionView : Model -> Section -> List (Html Msg)
sectionView model data =
    [ div
        [ class ("section section-" ++ data.kind)
        ]
        [ div [ class "container" ]
            [ div
                [ class "row" ]
                data.contents
            ]
        ]
    ]


stdGridView : Model -> List (Html Msg) -> List (Html Msg)
stdGridView model contents =
    [ grid []
        [ cell [ Grid.size All 12 ]
            contents
        ]
    ]


alertLinkView : Link -> Html Msg
alertLinkView link =
    a [ href "#", class "btn-sm  btn btn-primary btn-round", onClick link.action ] [ text link.title ]


quoteView : String -> String -> List Link -> Html Msg
quoteView sev message actions =
    blockquote []
        [ p [ class "blockquote blockquote-primary" ]
            [ text message
            , small []
                (actions
                    |> List.map alertLinkView
                )
            ]
        ]


alertView : String -> String -> List Link -> Html Msg
alertView sev message actions =
    div
        [ class "alert alert-warning"
        , attribute "role" "alert"
        ]
        [ div [ class "container clearfix" ]
            [ div [ class "alert-data float-left" ]
                [ strong [] [ text sev ]
                , text (" " ++ message)
                ]
            , div [ class "alert-actions float-right" ]
                (actions
                    |> List.map alertLinkView
                )
            ]
        ]


dialogView : String -> String -> List Link -> Html Msg
dialogView title message actions =
    div [ class "text-center" ]
        [ h3 [ class "title" ] [ text title ]
        , h5 [ class "description" ] [ text message ]
        , div [ class "alert-actions" ]
            (actions
                |> List.map alertLinkView
            )
        ]


errorView : Model -> String -> Html Msg
errorView model message =
    [ []
        |> alertView "danger" message
    ]
        |> basicSection model
        |> stdLayoutView model


loadingView : Model -> String -> List (Html Msg)
loadingView model title =
    [ p [ class "category text-center" ] [ text title ] ]


connectingView : Model -> Html Msg
connectingView model =
    "Connecting..."
        |> loadingView model
        |> basicSection model
        |> stdLayoutView model


registeringView : Model -> Html Msg
registeringView model =
    "Registering ..."
        |> loadingView model
        |> basicSection model
        |> stdLayoutView model


signingInView : Model -> Html Msg
signingInView model =
    "Logging in..."
        |> loadingView model
        |> basicSection model
        |> stdLayoutView model


loggingOutView : Model -> Html Msg
loggingOutView model =
    "Logging out..."
        |> loadingView model
        |> basicSection model
        |> stdLayoutView model


type alias Link =
    { title : String
    , icon : Maybe String
    , action : Msg
    , active : Bool
    , counter : Int
    }


simpleLink : String -> Msg -> Link
simpleLink t action =
    { title = t
    , icon = Nothing
    , action = action
    , active = True
    , counter = 0
    }


iconLink : String -> Msg -> String -> Link
iconLink t action icon =
    link t action True (Just icon) 0


activeIconLink : String -> Msg -> String -> Bool -> Link
activeIconLink t action icon active =
    link t action active (Just icon) 0


link : String -> Msg -> Bool -> Maybe String -> Int -> Link
link t a active i c =
    { title = t
    , icon = i
    , action = a
    , active = active
    , counter = c
    }


type alias CardDesc =
    { links : List Link
    , contents : List (Html Msg)
    }


activeClass : Bool -> String
activeClass active =
    case active of
        True ->
            "active"

        False ->
            ""


badge : Int -> Html Msg
badge count =
    case count of
        0 ->
            span [] []

        _ ->
            span [ class "badge badge-light" ] [ count |> toString |> text ]


badgeClass : Link -> String
badgeClass l =
    case l.counter of
        0 ->
            ""

        _ ->
            "with-badge"


linkViewContents : Link -> List (Html Msg)
linkViewContents link =
    case link.icon of
        Nothing ->
            [ span [ class "link-title" ] [ text link.title ]
            , badge link.counter
            ]

        Just icon ->
            case link.title of
                "" ->
                    [ link.icon |> iconOrDefault |> nucleoIcon ]

                _ ->
                    [ link.icon |> iconOrDefault |> nucleoIcon
                    , span [ class "link-title" ] [ text (" " ++ link.title) ]
                    , badge link.counter
                    ]


onClick : msg -> Attribute msg
onClick msg =
    onWithOptions "click" { stopPropagation = True, preventDefault = True } (Json.succeed msg)


linkView : Link -> Html Msg
linkView link =
    li [ class "nav-item" ]
        [ a
            [ onClick link.action
            , href "#"
            , class ("nav-link " ++ activeClass link.active ++ " " ++ badgeClass link)
            ]
            (link
                |> linkViewContents
            )
        ]


cardView : CardDesc -> Html Msg
cardView data =
    div [ class "card" ]
        [ ul [ class "nav nav-tabs nav-tabs-neutral justify-content-center", attribute "data-background-color" "orange" ]
            (List.map linkView data.links)
        , div [ class "card-block" ]
            [ div [ class "tab-content text-center" ]
                [ div [ class "tab-pane active" ]
                    data.contents
                ]
            ]
        ]


type alias SimpleCardDesc =
    { contents : List (Html Msg)
    , background : String
    , style : String
    }


simpleCardView : Model -> SimpleCardDesc -> List (Html Msg)
simpleCardView model data =
    [ div
        [ class ("card card-" ++ data.style)
        , attribute "data-background-color" data.background
        ]
        data.contents
    ]


type alias Form =
    { title : String
    , message : Maybe String
    , action : Msg
    , contents : List FormField
    , more : List Link
    , busy : Bool
    }


formAdditionalLinkView : Link -> Html Msg
formAdditionalLinkView link =
    h6 [] [ a [ class "btn btn-simple btn-primary btn-round", href "#", onClick link.action ] [ text link.title ] ]


faIcon : String -> Html Msg
faIcon style =
    "fa fa-" ++ style |> icon


nucleoIcon : String -> Html Msg
nucleoIcon style =
    "now-ui-icons " ++ style |> icon


spinningFaIcon : String -> Html Msg
spinningFaIcon style =
    "fa fa-" ++ style ++ " fa-spin fa-fw" |> icon


icon : String -> Html Msg
icon style =
    i [ class style ] []


inlineFormBusyIndicator : Form -> Html Msg
inlineFormBusyIndicator form =
    case form.busy of
        False ->
            span [] []

        True ->
            "circle-o-notch" |> spinningFaIcon


inlineFormView : Form -> Html Msg
inlineFormView data =
    div []
        ([ h3 [] [ text data.title ]
         , small [] [ data.message |> stringOrEmpty |> text ]
         ]
            ++ List.map mapInputView data.contents
            ++ [ a
                    [ href "#"
                    , class "btn btn-primary  btn-round"
                    , onClick data.action
                    ]
                    [ text data.title
                    , inlineFormBusyIndicator data
                    ]
               ]
        )


formView : Model -> Form -> List (Html Msg)
formView model data =
    [ Html.form [ class "text-center form" ]
        [ div [ class "header header-primary text-center" ]
            [ h4 [ class "title title-up" ]
                [ text data.title
                ]
            , small [] [ data.message |> stringOrEmpty |> text ]
            ]
        , div [ class "content" ]
            (List.map mapInputView data.contents)
        , div [ class "footer text-center" ]
            [ a
                [ href "#"
                , class "btn btn-neutral btn-round btn-lg"
                , onClick data.action
                ]
                [ text data.title ]
            ]
        , div [ class "footer text-center" ]
            (List.map
                formAdditionalLinkView
                data.more
            )
        ]
    ]


type FormFieldKind
    = Text
    | Password


type alias FormField =
    { title : String
    , value : String
    , icon : String
    , action : String -> Msg
    , kind : FormFieldKind
    }


inputView : String -> String -> List (Html Msg) -> Html Msg
inputView title icon contents =
    div [ class "input-group form-group-no-border" ]
        (span [ class "input-group-addon" ]
            [ i [ class ("now-ui-icons " ++ icon) ] []
            ]
            :: contents
        )


textInputView : FormField -> Html Msg
textInputView data =
    [ input
        [ type_ "text"
        , class "form-control"
        , placeholder data.title
        , onInput data.action
        ]
        []
    ]
        |> inputView data.title data.icon


passwordInputView : FormField -> Html Msg
passwordInputView data =
    [ input
        [ type_ "password"
        , class "form-control"
        , placeholder data.title
        , onInput data.action
        ]
        []
    ]
        |> inputView data.title data.icon


mapInputView : FormField -> Html Msg
mapInputView field =
    case field.kind of
        Text ->
            textInputView field

        Password ->
            passwordInputView field


materialLoginView : LoginData -> Model -> Html Msg
materialLoginView data model =
    { title = "Login"
    , form =
        { fields =
            [ { class = MDText, id = "1", title = "Email", icon = Nothing, action = UsernameChanged }
            , { class = MDPasswd, id = "2", title = "Password", icon = Nothing, action = PasswordChanged }
            ]
        , files = []
        , actions =
            [ { title = "Login", active = True, action = Login, icon = Nothing }
            ]
        }
    , links =
        [ { title = "Register"
          , active = False
          , action = ShowRegisterPage
          , icon = Nothing
          }
        ]
    }
        |> mdFormSectionView


materialRegisterView : RegisterData -> Model -> Html Msg
materialRegisterView data model =
    { title = "Register"
    , form =
        { fields =
            [ { class = MDText, id = "1", title = "First name", icon = Nothing, action = RegisterFirstChanged }
            , { class = MDText, id = "2", title = "Last name", icon = Nothing, action = RegisterLastChanged }
            , { class = MDText, id = "3", title = "Email", icon = Nothing, action = RegisterEmailChanged }
            , { class = MDPasswd, id = "2", title = "Password", icon = Nothing, action = RegisterPasswordChanged }
            ]
        , files = []
        , actions =
            [ { title = "Register", active = True, action = Register, icon = Nothing }
            ]
        }
    , links =
        [ { title = "Login"
          , active = False
          , action = ShowLoginPage
          , icon = Nothing
          }
        ]
    }
        |> mdFormSectionView


materialNavView : String -> List Link -> Html Msg
materialNavView title links =
    nav [ class "light-blue lighten-1", attribute "role" "navigation" ]
        [ div [ class "nav-wrapper container" ]
            (a [ id "logo-container", href "#", class "brand-logo" ]
                [ text title ]
                :: List.map materialNavLinkView links
            )
        ]


materialNavLinkView : Link -> Html Msg
materialNavLinkView link =
    ul [ class "right hide-on-med-and-down" ]
        [ li []
            [ a [ href "#" ]
                [ text link.title ]
            ]
        ]


materialFooterView : Html Msg
materialFooterView =
    footer [ class "page-footer orange" ]
        [ div [ class "container" ]
            [ div [ class "row" ]
                []
            ]
        , div [ class "footer-copyright" ]
            [ div [ class "container" ]
                [ text "Made by WeeKoNekt" ]
            ]
        ]


materialCardView : String -> Html Msg
materialCardView title =
    div [ class "row" ]
        [ div [ class "col s12 m6 offset-m3" ]
            [ div [ class "card" ]
                [ div [ class "card-content " ]
                    [ span [ class "card-title" ]
                        [ text title ]
                    ]
                ]
            ]
        ]



--<div class="row">
--        <div class="col s12 m6">
--          <div class="card blue-grey darken-1">
--            <div class="card-content white-text">
--              <span class="card-title">Card Title</span>
--              <p>I am a very simple card. I am good at containing small bits of information.
--              I am convenient because I require little markup to use effectively.</p>
--            </div>
--            <div class="card-action">
--              <a href="#">This is a link</a>
--              <a href="#">This is a link</a>
--            </div>
--          </div>
--        </div>
--      </div>
--  <nav class="light-blue lighten-1" role="navigation">
--   <div class="nav-wrapper container"><a id="logo-container" href="#" class="brand-logo">Logo</a>
--     <ul class="right hide-on-med-and-down">
--       <li><a href="#">Navbar Link</a></li>
--     </ul>
--     <ul id="nav-mobile" class="side-nav" style="transform: translateX(-100%);">
--       <li><a href="#">Navbar Link</a></li>
--     </ul>
--     <a href="#" data-activates="nav-mobile" class="button-collapse"><i class="material-icons">menu</i></a>
--   </div>
-- </nav>


loginView : LoginData -> Model -> Html Msg
loginView data model =
    { background = "orange"
    , style = "signup"
    , contents =
        { title = "Login"
        , message = data.message
        , action = Login
        , contents =
            [ { title = "Email"
              , value = data.username
              , icon = "ui-1_email-85"
              , action = UsernameChanged
              , kind = Text
              }
            , { title = "Password"
              , value = data.password
              , icon = "ui-1_lock-circle-open"
              , action = PasswordChanged
              , kind = Password
              }
            ]
        , more =
            [ registerLink
            ]
        , busy = False
        }
            |> formView model
    }
        |> simpleCardView model
        |> transparentSection model
        |> stdLayoutView model


registerLink : Link
registerLink =
    { title = "Create account"
    , action = ShowRegisterPage
    , icon = Nothing
    , active = True
    , counter = 0
    }


loginLink : Link
loginLink =
    { title = "Login"
    , action = ShowLoginPage
    , icon = Nothing
    , active = True
    , counter = 0
    }


registerView : RegisterData -> Model -> Html Msg
registerView data model =
    { background = "orange"
    , style = "signup"
    , contents =
        { title = "Create account"
        , message = data.message
        , action = Register
        , contents =
            [ { title = "Email"
              , value = data.email
              , icon = "ui-1_email-85"
              , action = RegisterEmailChanged
              , kind = Text
              }
            , { title = "First name"
              , value = data.first
              , icon = "users_circle-08"
              , action = RegisterFirstChanged
              , kind = Text
              }
            , { title = "Last name"
              , value = data.last
              , icon = "users_circle-08"
              , action = RegisterLastChanged
              , kind = Text
              }
            , { title = "Password"
              , value = data.password
              , icon = "ui-1_lock-circle-open"
              , action = RegisterPasswordChanged
              , kind = Password
              }
            ]
        , more =
            [ loginLink
            ]
        , busy = False
        }
            |> formView model
    }
        |> simpleCardView model
        |> basicSection model
        |> stdLayoutView model


type alias FormDesc =
    { title : String
    , action : Msg
    , message : Maybe String
    , main : List (Html Msg)
    , other : List (Html Msg)
    }


profileActionView : Link -> Html Msg
profileActionView link =
    case link.active of
        True ->
            a [ href "#", class "btn btn-primary btn-round", onClick link.action ]
                [ text link.title ]

        False ->
            span [] [ text link.title ]


presenceStyle : Dict String PresenceData -> String -> String
presenceStyle presence app =
    case Dict.get app presence of
        Nothing ->
            "offline"

        Just v ->
            v.status


userBadgeView : Link -> ProfileData -> Dict String PresenceData -> String -> Html Msg
userBadgeView link data presence app =
    div [ class ("user-badge user-" ++ presenceStyle presence app), onClick link.action ]
        [ a [ href "#" ]
            [ text (String.left 1 data.first ++ "" ++ String.left 1 data.last) ]
        ]


profileResultView : ProfileData -> List Link -> Html Msg
profileResultView data actions =
    div [ class "col-md-4" ]
        [ div [ class "team-player text-center" ]
            ([ img [ class "rounded-circle img-fluid img-raised" ] []
             , h5 []
                [ text (data.first ++ " " ++ data.last)
                ]
             , p [ class "category text-primary" ] [ text "Traveller" ]
             , p [ class "description" ] []
             ]
                ++ (actions
                        |> List.map profileActionView
                   )
            )
        ]


type alias HSplit =
    { ratio : Int
    , left : List (Html Msg)
    , right : List (Html Msg)
    }


type alias VSplit =
    { top : List (Html Msg)
    , bottom : List (Html Msg)
    }


vSplitView : Model -> VSplit -> List (Html Msg)
vSplitView model split =
    [ grid []
        [ cell [ Grid.size All 12 ]
            split.top
        ]
    , grid []
        [ cell [ Grid.size All 12 ]
            split.bottom
        ]
    ]


logoutMessage : Model -> String
logoutMessage model =
    case model.profile of
        Nothing ->
            "Logout"

        Just p ->
            "Logout " ++ toSentenceCase p.first


userView : Model -> List (Html Msg) -> Html Msg
userView model contents =
    contents
        |> graySection model
        |> navLayoutView model
            { title = "Weekonekt"
            , links =
                [ iconLink "Logout" Logout "media-1_button-power"
                , iconLink "Network" ShowGroupsPage "objects_planet"
                ]
            }


searchBoxView : String -> String -> (String -> Msg) -> Msg -> Html Msg
searchBoxView title value onInput onSubmit =
    div []
        (List.map mapInputView
            [ { title = title
              , value = value
              , icon = ""
              , action = onInput
              , kind = Text
              }
            ]
        )


globalMenu : Model -> List Link
globalMenu model =
    [ activeIconLink "Groups" ShowGroupsPage "objects_planet" (model.state == Groups)
    , activeIconLink "People" ShowPeoplePage "users_circle-08" (model.state == People)
    , activeIconLink "Messages" NoOp "ui-1_email-85" False
    , activeIconLink "Talk" ShowTalkPage "media-2_sound-wave" (model.state == Talk)
    , activeIconLink "Events" NoOp "location_pin" False
    , activeIconLink "Passes" NoOp "shopping_tag-content" False
    , activeIconLink "Hotels" NoOp "shopping_shop" False
    , activeIconLink "Lifts" NoOp "transportation_bus-front-12" False
    , activeIconLink "Settings" NoOp "ui-1_settings-gear-63" False
    , activeIconLink "Logout" Logout "media-1_button-power" False
    ]


groupsLocalView : Model -> List (Html Msg)
groupsLocalView model =
    [ searchBoxView "Enter a group name" "" SearchKeywordsChanged NoOp
    , [ { span = 6
        , align = Center
        , background = "transparent"
        , contents =
            [ { style = "neutral wide"
              , title = "Search"
              , action = SearchGroup
              , centered = True
              , icon = Nothing
              }
                |> buttonView
            ]
        }
      , { span = 6
        , align = Center
        , background = "transparent"
        , contents =
            [ { style = "neutral wide"
              , title = "New group"
              , action = CreateGroup
              , centered = True
              , icon = Nothing
              }
                |> buttonView
            ]
        }
      ]
        |> colsLayout
    , groupListView model "none"
    ]


mainSection : List (Html Msg) -> Html Msg
mainSection contents =
    div [ class "row" ]
        [ div [ class "col-12", style [ ( "padding-right", "30px" ) ] ]
            [ div [ class "layout-main-section" ]
                contents
            ]
        ]


emptyMainView : List (Html Msg)
emptyMainView =
    [ [] |> mainSection ]


groupInviteFormView : Html Msg
groupInviteFormView =
    searchBoxView "Enter an email address" "" SearchKeywordsChanged NoOp


groupMainView : Model -> List (Html Msg)
groupMainView model =
    case model.group of
        Nothing ->
            [ p [] [ text "No group selected" ] ]

        Just g ->
            [ [ h5 [] [ text g.name ] ] |> mainSection
            , [ h5 [] [ text "Invite" ]
              , groupInviteFormView
              , { style = "primary wide"
                , title = "Search people"
                , action = SearchPeople
                , centered = True
                , icon = Nothing
                }
                    |> buttonView
              , model.people
                    |> inviteUsersView g
              ]
                |> mainSection
            ]


materialEmptyView : Html Msg
materialEmptyView =
    div [] []


materialSideLinks : String -> List MDLink
materialSideLinks active =
    [ { title = "Groups"
      , active = "groups" == active
      , action = ShowGroupsPage
      , icon =
            Just
                { name = "group"
                , position = "left"
                }
      }
    , { title = "People"
      , active = "people" == active
      , action = ShowPeoplePage
      , icon =
            Just
                { name = "contacts"
                , position = "left"
                }
      }
    ]


materialGroupsMasterView : Model -> Html Msg
materialGroupsMasterView model =
    { fields = []
    , files =
        [ { id = "1", title = "Select file", icon = Nothing, action = FilesSelect }
        ]
    , actions =
        [ { title = "Upload"
          , active = True
          , action = Upload
          , icon =
                Just
                    { name = "cloud_upload"
                    , position = "left"
                    }
          }
        ]
    }
        |> mdFormView


materialGroupsView : Model -> Html Msg
materialGroupsView model =
    let
        nav =
            materialEmptyView

        side =
            "groups" |> materialSideLinks |> mdLinkCollectionView

        main =
            model |> materialGroupsMasterView
    in
    mdSideNavLayout nav side main


groupsView : Model -> Html Msg
groupsView model =
    adminLayoutView
        (globalMenu model)
        (groupsLocalView model)
        (groupMainView model)


contactListItemView : UserView -> Html Msg
contactListItemView v =
    case v.user of
        Nothing ->
            div [] []

        Just u ->
            div [ class "layout-local-item layout-local-item-contact" ]
                [ userBadgeView (simpleLink "" NoOp) u v.presence "none"
                ]


contactsListView : Model -> Html Msg
contactsListView model =
    div [ class "layout-local-list" ]
        (model.groups
            |> Dict.values
            |> List.concatMap
                (\g ->
                    g.participants
                        |> Dict.values
                )
            |> List.map contactListItemView
        )


peopleLocalView : Model -> List (Html Msg)
peopleLocalView model =
    [ searchBoxView "Enter a person's name" "" SearchKeywordsChanged NoOp
    , contactsListView model
    ]


peopleView : Model -> Html Msg
peopleView model =
    adminLayoutView
        (globalMenu model)
        (peopleLocalView model)
        emptyMainView


talkLocalView : Model -> List (Html Msg)
talkLocalView model =
    [ searchBoxView "Filter rooms" "" SearchKeywordsChanged NoOp
    , groupListView model "conferencing"
    ]


talkView : Model -> Html Msg
talkView model =
    adminLayoutView
        (globalMenu model)
        (talkLocalView model)
        []


groupInfoView : GroupData -> Html Msg
groupInfoView g =
    h4 []
        [ text g.name
        ]


groupInviteForm : Html Msg
groupInviteForm =
    div []
        (List.map mapInputView
            [ { title = "Invite someone"
              , value = ""
              , icon = ""
              , action = SearchKeywordsChanged
              , kind = Text
              }
            ]
        )


groupMembersView : GroupData -> Html Msg
groupMembersView g =
    div []
        [ h6 []
            [ text "Members here" ]
        , groupInviteForm
        ]


groupConferenceView : GroupData -> Html Msg
groupConferenceView g =
    div []
        [ { style = "primary"
          , title = "Join"
          , action = Capabilities
          , centered = True
          , icon = Just "gestures_tap-01"
          }
            |> buttonView
        ]


groupParticipantView : String -> UserView -> Html Msg
groupParticipantView app v =
    case v.user of
        Nothing ->
            div [] []

        Just u ->
            div [ class "layout-local-item-participant" ]
                [ userBadgeView (simpleLink "" NoOp) u v.presence app
                ]


groupParticipantsView : String -> Dict String UserView -> Html Msg
groupParticipantsView app participants =
    div [ class "group-participants" ]
        (List.map (groupParticipantView app) (Dict.values participants))


groupListItemView : String -> GroupView -> Html Msg
groupListItemView app v =
    case v.invite of
        Nothing ->
            div [ class "layout-local-item layout-local-item-group", onClick (SelectGroup v.group) ]
                [ p [] [ text v.group.name ]
                , groupParticipantsView app v.participants
                ]

        Just i ->
            div [ class "layout-local-item layout-local-item-group" ]
                [ p []
                    [ text
                        (i.from.first
                            ++ " is inviting you to join "
                            ++ v.group.name
                        )
                    ]
                , div [ class "layout-local-item-actions" ]
                    [ { title = "Accept"
                      , icon = Just "check"
                      , action = AcceptInvite i.id
                      , centered = True
                      , style = "neutral btn-sm"
                      }
                        |> buttonView
                    , { title = "Decline"
                      , icon = Just "ban"
                      , action = DeclineInvite i.id
                      , centered = True
                      , style = "neutral btn-sm"
                      }
                        |> buttonView
                    ]
                ]


groupListView : Model -> String -> Html Msg
groupListView model app =
    div [ class "layout-local-list" ]
        (model.groups
            |> Dict.values
            |> List.map (groupListItemView app)
        )


searchView : Model -> SearchData -> Html Msg
searchView model searchData =
    [ { span = 12
      , background = "transparent"
      , align = Left
      , contents =
            [ searchData
                |> searchPeopleForm False
            ]
                |> rowsLayoutView
      }
    ]
        |> colsLayoutView model
        |> userView model


searchResultsTitleView : SearchData -> Html Msg
searchResultsTitleView searchData =
    h3 [] [ text ("Search results for \"" ++ searchData.keywords ++ "\"") ]


searchingView : Model -> SearchData -> Html Msg
searchingView model searchData =
    [ { span = 12
      , background = "transparent"
      , align = Left
      , contents =
            [ searchData |> searchPeopleForm True
            ]
                |> rowsLayoutView
      }
    ]
        |> colsLayoutView model
        |> userView model


searchPeopleForm : Bool -> SearchData -> Html Msg
searchPeopleForm busy data =
    { action = SearchPeople
    , title = "Search people"
    , contents =
        [ { title = "Name or email"
          , value = data.keywords
          , icon = "users_circle-08"
          , action = SearchKeywordsChanged
          , kind = Text
          }
        ]
    , message = data.message
    , busy = busy
    , more = []
    }
        |> inlineFormView


inviteUsersView : GroupData -> List ProfileData -> Html Msg
inviteUsersView group people =
    div [ class "layout-main-list list-users" ]
        (List.map
            (\u ->
                div [ class "layout-main-list-item layout-main-list-item-user" ]
                    [ userBadgeView (simpleLink "Invite" (Invite group u)) u Dict.empty "none" ]
            )
            people
        )


notificationsView : Model -> Html Msg
notificationsView model =
    div [ id "notifications", class "section-notifications" ]
        [ receivedInvitesView model
        , sentInvitesView model
        ]


receivedInvitesView : Model -> Html Msg
receivedInvitesView model =
    div []
        (List.map
            receivedInviteView
            (model.invitesReceived
                |> allInvites "new"
            )
        )


sentInvitesView : Model -> Html Msg
sentInvitesView model =
    div []
        (List.map
            sentInviteView
            (model.invitesSent
                |> allInvitesExcept "new"
            )
        )


allInvitesExcept : String -> List InviteData -> List InviteData
allInvitesExcept status invites =
    invites
        |> List.filter
            (\i ->
                i.status /= status
            )


allInvites : String -> List InviteData -> List InviteData
allInvites status invites =
    invites
        |> List.filter
            (\i ->
                i.status == status
            )


receivedInviteView : InviteData -> Html Msg
receivedInviteView data =
    [ { title = "Accept"
      , icon = Just "check"
      , action = AcceptInvite data.id
      , active = True
      , counter = 0
      }
    , { title = "Decline"
      , icon = Just "ban"
      , action = DeclineInvite data.id
      , active = True
      , counter = 0
      }
    ]
        |> alertView "info" (data.from.first ++ " is inviting you to connect")


sentInviteView : InviteData -> Html Msg
sentInviteView data =
    [ { title = "Dismiss"
      , icon = Just ""
      , action = AckInvite data.id
      , active = True
      , counter = 0
      }
    ]
        |> alertView "info" (data.from.first ++ " " ++ data.status ++ " your invite")


contactView : ProfileData -> Html Msg
contactView data =
    [ { title = "Message"
      , icon = Just "comment"
      , action = NoOp
      , active = True
      , counter = 0
      }
    , { title = "Call"
      , icon = Just "video-camera"
      , action = NoOp
      , active = True
      , counter = 0
      }
    ]
        |> profileResultView data


iconOrDefault : Maybe String -> String
iconOrDefault icon =
    case icon of
        Nothing ->
            ""

        Just i ->
            i


mapBool : Bool -> String -> String -> String
mapBool b v1 v2 =
    case b of
        True ->
            v1

        False ->
            v2


navPillView : Link -> Html Msg
navPillView link =
    li [ class "nav-item" ]
        [ a
            [ onClick link.action
            , class ("nav-link " ++ activeClass link.active)
            , href "#"
            ]
            [ link.icon |> iconOrDefault |> nucleoIcon ]
        ]


navPillsView : Bool -> List Link -> Html Msg
navPillsView vertical links =
    links
        |> List.map navPillView
        |> ul
            [ class
                ("nav nav-pills nav-pills-primary "
                    ++ mapBool vertical "nav-vertical" ""
                )
            ]


buttonListView : List Link -> Html Msg
buttonListView links =
    links
        |> List.map linkButtonView
        |> div [ class "button-list-view" ]


avatarView : Model -> Html Msg
avatarView model =
    div [ class "avatar-view" ]
        [ i [ class "fa fa-user 2x" ] [] ]


welcomeView : Model -> Html Msg
welcomeView model =
    case model.profile of
        Nothing ->
            span [] []

        Just p ->
            h5 [ class "description" ]
                [ text ("Hello " ++ p.first) ]


sidebarView : Model -> List (Html Msg)
sidebarView model =
    let
        invites =
            model.invitesReceived
                |> invitesInStatus "new"
                |> List.length
    in
    [ avatarView model
    , welcomeView model
    , [ link "Search people" ShowSearchPage True Nothing 0
      , link "Settings" NoOp True Nothing 0
      , link "Logout" Logout True Nothing 0
      ]
        |> buttonListView
    ]



--[ userAccountView model ]


userAccountView : Model -> Html Msg
userAccountView model =
    div []
        [ div []
            [ { style = "primary"
              , title = logoutMessage model
              , action = Logout
              , centered = True
              , icon = Just "media-1_button-power"
              }
                |> buttonView
            ]
        , div [ style [ ( "margin-top", "10px" ) ] ]
            [ { style = "simple"
              , title = "Settings"
              , action = NoOp
              , centered = True
              , icon = Just "ui-1_settings-gear-63"
              }
                |> buttonView
            ]
        ]


joiningView : Model -> Html Msg
joiningView model =
    Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            ]
            { header = [ header model ]
            , drawer = []
            , tabs =
                ( [ text "About"
                  ]
                , [ Color.background (Color.color Color.Teal Color.S400) ]
                )
            , main =
                [ grid [ css "padding" "2rem" ]
                    [ cell [ Grid.size All 4 ]
                        [ styled p [ Typo.display1 ] [ text "Joining..." ]
                        ]
                    ]
                ]
            }


header : Model -> Html Msg
header model =
    styled div
        [ css "padding" "2rem" ]
        [ styled p
            [ Typo.display1 ]
            [ text "WebRTC with Elm" ]
        , styled p
            [ Typo.title, Typo.uppercase ]
            [ text "A test project" ]
        ]


roomView : RoomData -> Model -> Html Msg
roomView room model =
    Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader

            --, Layout.fixedDrawer
            ]
            { header = [ header model ]

            --, drawer = [ drawerHeader model, drawerBody model ]
            , drawer = []
            , tabs =
                ( [ text "About"
                  ]
                , [ Color.background (Color.color Color.Teal Color.S400) ]
                )
            , main =
                [ grid [ css "padding" "2rem" ]
                    [ cell [ Grid.size All 12 ]
                        [ styled p [ Typo.title ] [ text model.session ] ]
                    ]
                , grid [ css "padding" "2rem" ]
                    [ cell [ Grid.size All 12 ]
                        [ styled p [ Typo.display1 ] [ text "Room ready" ]
                        , p [] [ text ("Participants: " ++ toString room.participants) ]
                        , grid []
                            [ cell [ Grid.size All 1 ]
                                [ Button.render Mdl
                                    [ 0 ]
                                    model.mdl
                                    [ Button.raised
                                    , Button.colored
                                    , Options.onClick StartVideoCall
                                    ]
                                    [ text "Video call" ]
                                ]
                            , cell [ Grid.size All 1 ]
                                [ Button.render Mdl
                                    [ 0 ]
                                    model.mdl
                                    [ Button.raised
                                    , Button.colored

                                    --, Options.onClick MyClickMsg
                                    ]
                                    [ text "Audio call" ]
                                ]
                            ]
                        ]
                    ]
                ]
            }


createdView : RoomData -> Model -> Html Msg
createdView room model =
    Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            ]
            { header = [ header model ]
            , drawer = []
            , tabs =
                ( [ text "About"
                  ]
                , [ Color.background (Color.color Color.Teal Color.S400) ]
                )
            , main =
                [ grid [ css "padding" "2rem" ]
                    [ cell [ Grid.size All 4 ]
                        [ styled p [ Typo.display1 ] [ text "Waiting for peer" ]
                        , p [] [ text "You are the only participant" ]
                        ]
                    ]
                ]
            }


drawerHeader : Model -> Html Msg
drawerHeader model =
    h1 [] [ text "Drawer Header" ]


drawerBody : Model -> Html Msg
drawerBody model =
    p [] [ text "Drawer body" ]


startingVideoView : Model -> Html Msg
startingVideoView model =
    Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            ]
            { header = [ header model ]
            , drawer = []
            , tabs =
                ( [ text "About"
                  ]
                , [ Color.background (Color.color Color.Teal Color.S400) ]
                )
            , main =
                [ grid [ css "padding" "2rem" ]
                    [ cell [ Grid.size All 4 ]
                        [ styled p [ Typo.display1 ] [ text "Starting video call..." ]
                        ]
                    ]
                ]
            }


terminatingVideoCallView : Model -> Html Msg
terminatingVideoCallView model =
    Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            ]
            { header = [ header model ]
            , drawer = []
            , tabs =
                ( [ text "About"
                  ]
                , [ Color.background (Color.color Color.Teal Color.S400) ]
                )
            , main =
                [ grid [ css "padding" "2rem" ]
                    [ cell [ Grid.size All 4 ]
                        [ styled p [ Typo.display1 ] [ text "Terminating video call..." ]
                        ]
                    ]
                ]
            }


callStartingView : Model -> Html Msg
callStartingView model =
    Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            ]
            { header = [ header model ]
            , drawer = []
            , tabs =
                ( [ text "About"
                  ]
                , [ Color.background (Color.color Color.Teal Color.S400) ]
                )
            , main =
                [ grid [ css "padding" "2rem" ]
                    [ cell [ Grid.size All 4 ]
                        [ styled p [ Typo.display1 ] [ text "Starting call..." ]
                        ]
                    ]
                ]
            }


incomingCallView : RoomData -> MediaInfo -> Model -> Html Msg
incomingCallView room media model =
    Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            ]
            { header = [ header model ]
            , drawer = []
            , tabs =
                ( [ text "About"
                  ]
                , [ Color.background (Color.color Color.Teal Color.S400) ]
                )
            , main =
                [ grid [ css "padding" "2rem" ]
                    [ cell [ Grid.size All 4 ]
                        [ styled p [ Typo.display1 ] [ text "Incoming Call..." ]
                        , grid []
                            [ cell [ Grid.size All 1 ]
                                [ Button.render Mdl
                                    [ 0 ]
                                    model.mdl
                                    [ Button.raised
                                    , Button.colored
                                    , Options.onClick AcceptCall
                                    ]
                                    [ text "Accept" ]
                                , Button.render Mdl
                                    [ 0 ]
                                    model.mdl
                                    [ Button.raised
                                    , Button.colored
                                    , Options.onClick DeclineCall
                                    ]
                                    [ text "Decline" ]
                                ]
                            ]
                        ]
                    ]
                ]
            }


outgoingCallView : RoomData -> MediaInfo -> Model -> Html Msg
outgoingCallView room localMedia model =
    Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader

            --, Layout.fixedDrawer
            ]
            { header = [ header model ]

            --, drawer = [ drawerHeader model, drawerBody model ]
            , drawer = []
            , tabs =
                ( [ text "About"
                  ]
                , [ Color.background (Color.color Color.Teal Color.S400) ]
                )
            , main =
                [ grid [ css "padding" "2rem" ]
                    [ cell [ Grid.size All 6 ]
                        [ styled p [ Typo.display1 ] [ text "Stream" ]
                        , p []
                            [ videoView localMedia.url
                            ]
                        ]
                    , cell [ Grid.size All 6 ]
                        [ sdpView "Local SDP" localMedia.sdp
                        , iceView "Local Ice Candidates" localMedia.ice
                        , grid []
                            [ cell [ Grid.size All 1 ]
                                [ Button.render Mdl
                                    [ 0 ]
                                    model.mdl
                                    [ Button.raised
                                    , Button.colored
                                    , Options.onClick TerminateVideoCall
                                    ]
                                    [ text "Hangup" ]
                                ]
                            ]
                        ]
                    ]
                ]
            }


callView : RoomData -> MediaInfo -> MediaInfo -> Model -> Html Msg
callView room localMedia remoteMedia model =
    Material.Scheme.topWithScheme Color.Teal Color.LightGreen <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader

            --, Layout.fixedDrawer
            ]
            { header = [ header model ]

            --, drawer = [ drawerHeader model, drawerBody model ]
            , drawer = []
            , tabs =
                ( [ text "About"
                  ]
                , [ Color.background (Color.color Color.Teal Color.S400) ]
                )
            , main =
                [ grid [ css "padding" "2rem" ]
                    [ cell [ Grid.size All 5 ]
                        [ streamView "Remote stream" remoteMedia model
                        ]
                    , cell [ Grid.size All 5 ]
                        [ streamView "Local stream" localMedia model
                        ]
                    , cell [ Grid.size All 2 ]
                        [ { title = "Hangup"
                          , action = TerminateVideoCall
                          , style = "primary"
                          , centered = False
                          , icon = Nothing
                          }
                            |> buttonView
                        ]
                    ]
                ]
            }


simpleMessageView : Maybe String -> Model -> Html Msg
simpleMessageView mess model =
    Options.styled p
        [ Typo.body1 ]
        [ mess
            |> stringOrEmpty
            |> text
        ]


type alias Button =
    { style : String
    , action : Msg
    , title : String
    , centered : Bool
    , icon : Maybe String
    }


buttonView : Button -> Html Msg
buttonView b =
    button [ class ("btn btn-round btn-" ++ b.style), type_ "button", onClick b.action ]
        ({ title = b.title
         , icon = b.icon
         , active = True
         , action = NoOp
         , counter = 0
         }
            |> linkViewContents
        )


miniButtonView : Button -> Html Msg
miniButtonView b =
    button [ class ("btn btn-icon  btn-icon-mini btn-round btn btn-" ++ b.style), type_ "button", onClick b.action ]
        ({ title = ""
         , icon = b.icon
         , active = True
         , action = NoOp
         , counter = 0
         }
            |> linkViewContents
        )


linkButtonView : Link -> Html Msg
linkButtonView l =
    a [ href "#", class ("btn btn-round btn-primary " ++ badgeClass l), onClick l.action ]
        (l
            |> linkViewContents
        )


buttonLinkView : String -> Msg -> Model -> Html Msg
buttonLinkView title msg model =
    Button.render Mdl
        [ 0 ]
        model.mdl
        [ Button.ripple
        , Options.onClick msg
        ]
        [ text title ]


textFieldView : Int -> String -> String -> (String -> Msg) -> Model -> Html Msg
textFieldView index title value onInput model =
    Textfield.render Mdl
        [ index ]
        model.mdl
        [ Textfield.label title

        --        , Textfield.floatingLabel
        , Textfield.text_
        , Textfield.value value
        , Options.onInput
            onInput
        ]
        []


passwordFieldView : Int -> String -> String -> (String -> Msg) -> Model -> Html Msg
passwordFieldView index title value onInput model =
    Textfield.render Mdl
        [ index ]
        model.mdl
        [ Textfield.label title
        , Textfield.value value

        --, Textfield.floatingLabel
        , Textfield.password
        , Options.onInput onInput
        ]
        []


streamView : String -> MediaInfo -> Model -> Html Msg
streamView title media model =
    div []
        [ styled p [ Typo.display1 ] [ text title ]
        , videoView media.url
        , sdpView "SDP" media.sdp
        , iceView "Ice candidates" media.ice
        ]


videoView : Maybe String -> Html Msg
videoView url =
    case url of
        Nothing ->
            p [] [ text "Stream not yet available" ]

        Just u ->
            video
                [ style [ ( "width", "100%" ) ]
                , src u
                , autoplay True
                ]
                []


sdpView : String -> Maybe String -> Html Msg
sdpView title sdp =
    div []
        [ styled p [ Typo.display1 ] [ text title ]
        , p [] [ text (sdpText sdp) ]
        ]


sdpText : Maybe String -> String
sdpText sdp =
    case sdp of
        Nothing ->
            "No sdp available"

        Just s ->
            s


iceView : String -> List String -> Html Msg
iceView title ice =
    div []
        [ styled p [ Typo.display1 ] [ text title ]
        , p []
            (List.map
                (\i ->
                    p [] [ text i ]
                )
                ice
            )
        ]
