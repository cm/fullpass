module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import WebSocket exposing (..)
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)
import Time exposing (..)


type alias Flags =
    { apiHost : String
    , oauth : String
    , redirectUri : String
    , client : String
    , sessionId : Maybe String
    }


type State
    = LoggedOut
    | LoggingIn
    | LoggedIn
    | Error


type alias Profile =
    { first_name : String
    , last_name : String
    , picture : String
    }


type alias Model =
    { flags : Flags
    , state : State
    , message : String
    , profile : Maybe Profile
    }


type alias Ping =
    { message : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    case flags |> getSessionId of
        Nothing ->
            ( { flags = flags, state = LoggedOut, message = "", profile = Nothing }
            , Cmd.none
            )

        Just id ->
            ( { flags = flags, state = LoggingIn, message = "", profile = Nothing }
            , getSession id flags
            )


getSession : String -> Flags -> Cmd Msg
getSession id flags =
    WebSocket.send (wsUrl flags) (encodeGetSession id)


getSessionId : Flags -> Maybe String
getSessionId flags =
    case flags.sessionId of
        Nothing ->
            Nothing

        Just s ->
            case s |> String.length of
                0 ->
                    Nothing

                _ ->
                    Just s


encodeText : String -> String
encodeText str =
    Encode.encode 0
        (Encode.object
            [ ( "message", Encode.string str )
            ]
        )


encodeAction : String -> String
encodeAction str =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string str )
            ]
        )


encodeGetSession : String -> String
encodeGetSession id =
    Encode.encode 0
        (Encode.object
            [ ( "action", Encode.string "session" )
            , ( "id", Encode.string id )
            ]
        )


wsUrl : Flags -> String
wsUrl flags =
    "ws://" ++ flags.apiHost ++ "/ws"


type Msg
    = NoOp
    | WsMsg String
    | NotImplemented
    | PingData Ping
    | ProfileData Profile
    | WsPing Time
    | WsError String


view : Model -> Html Msg
view model =
    case model.state of
        LoggedIn ->
            loggedInPage model

        LoggingIn ->
            loggingInPage model

        LoggedOut ->
            welcomePage model

        Error ->
            errorPage model


welcomePage : Model -> Html Msg
welcomePage model =
    div []
        [ headerSection model
        , promoSection model
        , aboutSection model
        , footerSection model
        ]


loggingInPage : Model -> Html Msg
loggingInPage model =
    div []
        [ headerSection model
        , loggingInSection model
        , footerSection model
        ]


loggedInPage : Model -> Html Msg
loggedInPage model =
    div []
        [ headerSection model
        , footerSection model
        ]


errorPage : Model -> Html Msg
errorPage model =
    div []
        [ headerSection model
        , errorSection model
        , footerSection model
        ]


headerSection : Model -> Html Msg
headerSection model =
    header [ id "header", class "header" ]
        [ div [ class "container" ]
            [ h1 [ class "logo pull-left" ]
                [ a [ class "scrollto", href "#promo" ]
                    [ span [ class "logo-title" ]
                        [ text "fullpass.in" ]
                    ]
                ]
            , nav [ id "main-nav", class "main-nav navbar-right", role "navigation" ]
                [ div [ class "navbar-header" ]
                    [ button
                        [ class "navbar-toggle collapsed"
                        , type_ "button"
                        , attribute "data-toggle" "collapse"
                        , attribute "data-target" "#navbar-collapse"
                        , ariaExpanded "false"
                        ]
                        [ span [ class "sr-only" ] [ text "Toggle navigation" ]
                        , span [ class "icon-bar" ] []
                        , span [ class "icon-bar" ] []
                        , span [ class "icon-bar" ] []
                        ]
                    ]
                , div
                    [ id "navbar-collapse"
                    , class "navbar-collapse collapse"
                    , ariaExpanded "true"
                    , style [ ( "height", "1px" ) ]
                    ]
                    [ ul [ class "nav navbar-nav" ]
                        [ li [ class "nav-item sr-only" ]
                            [ a [ class "scrollto", href "#promo" ]
                                [ text "Home" ]
                            ]
                        , li [ class "nav-item last" ]
                            [ a [ class "scrollto", href "#about" ]
                                [ text "About" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


promoSection : Model -> Html Msg
promoSection model =
    section [ id "promo", class "promo section offset-header" ]
        [ div [ class "container text-center" ]
            [ h2 [ class "title" ]
                [ text "fullpass"
                , span [ class "highlight" ]
                    [ text ".in" ]
                ]
            , p [ class "intro" ]
                [ text "Your last minute Kizomba full pass and hotel"
                , br [] []
                , text "Works with Salsa and Bachata too"
                ]
            , loginSection (model)
            , ul [ class "meta list-inline" ]
                []
            ]
        ]


loginSection : Model -> Html Msg
loginSection model =
    case model.state of
        LoggingIn ->
            div []
                [ text "Logging in..."
                ]

        _ ->
            div [ class "btns" ]
                [ a [ class "btn btn-cta-primary", href (fbLoginUrl model) ]
                    [ i [ class "fa fa-facebook-official" ] []
                    , text " Login with Facebook"
                    ]
                ]


fbLoginUrl : Model -> String
fbLoginUrl model =
    model.flags.oauth
        ++ "?client_id="
        ++ model.flags.client
        ++ "&redirect_uri="
        ++ model.flags.redirectUri


aboutSection : Model -> Html Msg
aboutSection model =
    section [ id "about", class "about section" ]
        [ div [ class "container" ]
            [ h2 [ class "title text-center" ] [ text "What is fullpass.in?" ]
            , p [ class "intro text-center" ]
                [ text "fullpass.in is the most convenient way to buy or sell your full pass or, look for the best room mate for your next dancing event" ]
            , div [ class "row" ]
                (List.map
                    aboutSectionInfo
                    [ ( "user-secret", "Privacy saver", "Unlike when you post a message to a Facebook event, with fullpass.in your identity is kept secret. You won't get unsolicited private messages or friend requests. You will only interact with that person who is actually interested in your full pass or accomodation sharing. At the right time." )
                    , ( "clock-o", "Time saver", "No need to spend time actively browsing for specific posts in public Facebook events, or reading private messages that can be easily missed. With fullpass.in, you just tell us what you are looking for, and we will notify you as soon as we find a good match." )
                    , ( "dollar", "Money saver", "With fullpass.in, you specify how much money you want to spend or you want to obtain for your full pass or hotel reservation. Besides, by helping you in findin a buyer or a room mate, we are able to mitigate your expenses. On the other side, you might also find an interesting opportunity price." )
                    ]
                )
            ]
        ]


aboutSectionInfo : ( String, String, String ) -> Html Msg
aboutSectionInfo ( icon, title, desc ) =
    div [ class "item col-md-4 col-sm-6 col-xs-12" ]
        [ div [ class "icon-holder" ]
            [ i [ class ("fa fa-" ++ icon) ] [] ]
        , div [ class "content" ]
            [ h3 [ class "sub-title" ]
                [ text title ]
            , p []
                [ text desc ]
            ]
        ]


featuresSection : Model -> Html Msg
featuresSection model =
    section [ id "features", class "features section" ] []


loggingInSection : Model -> Html Msg
loggingInSection model =
    section [ id "logging-in", class "features section full-height" ]
        [ div [ class "contact container" ]
            [ div [ class "author-message" ]
                [ div [ class "bubble" ]
                    [ h3 [ class "sub-title text-center" ]
                        [ text "Logging in..." ]
                    , p [ class "text-center" ]
                        [ text "Please wait while we check your session" ]
                    ]
                ]
            ]
        ]


errorSection : Model -> Html Msg
errorSection model =
    section [ id "error", class "features section full-height" ]
        [ div [ class "contact container" ]
            [ div [ class "author-message" ]
                [ div [ class "bubble" ]
                    [ h3 [ class "sub-title text-center" ] [ text "Something weird happened!" ]
                    , p [ class "text-center" ]
                        [ text model.message ]
                    ]
                ]
            ]
        ]


docsSection : Model -> Html Msg
docsSection model =
    section [ id "docs", class "docs section" ] []


licenseSection : Model -> Html Msg
licenseSection model =
    section [ id "license", class "license section" ] []


contactSection : Model -> Html Msg
contactSection model =
    section [ id "contact", class "contact section has-pattern" ] []


footerSection : Model -> Html Msg
footerSection model =
    footer [ class "footer" ]
        [ div [ class "container text-center" ]
            [ small [ class "copyright" ]
                [ text "Made with "
                , i [ class "fa fa-heart" ] []
                , text " for Kizomba Addicts"
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NotImplemented ->
            ( model, Cmd.none )

        ProfileData p ->
            ( { model | profile = Just p, state = LoggedIn }, Cmd.none )

        PingData p ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        WsMsg str ->
            case (decodeString wsDecoder str) of
                Ok msg2 ->
                    update msg2 model

                Err e ->
                    update (WsError (toString e)) model

        WsPing _ ->
            ( model, ping model.flags )

        WsError reason ->
            ( { model | message = reason, state = Error }, Cmd.none )


ping : Flags -> Cmd Msg
ping flags =
    WebSocket.send (wsUrl flags) (encodeAction "ping")


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (Time.second * 15) WsPing
        , WebSocket.listen (wsUrl model.flags) WsMsg
        , WebSocket.keepAlive (wsUrl model.flags)
        ]


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


wsDecoder : Decoder Msg
wsDecoder =
    (field "action" Decode.string) |> andThen wsDecoderByAction


wsDecoderByStatus : String -> Decoder Msg
wsDecoderByStatus status =
    case status of
        "ok" ->
            (field "type" Decode.string)
                |> andThen wsDecoderByAction

        _ ->
            Decode.fail <| status


wsDecoderByAction : String -> Decoder Msg
wsDecoderByAction action =
    case action of
        "ping" ->
            Decode.map PingData
                (field "data" pingDecoder)

        "session" ->
            Decode.map ProfileData
                (field "data" profileDecoder)

        _ ->
            Decode.fail <| action


pingDecoder : Decoder Ping
pingDecoder =
    Decode.map Ping
        (field "message" Decode.string)


profileDecoder : Decoder Profile
profileDecoder =
    Decode.map3 Profile
        (field "picture" Decode.string)
        (field "last_name" Decode.string)
        (field "first_name" Decode.string)
