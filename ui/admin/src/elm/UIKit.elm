module UIKit exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Messages exposing (..)
import Models exposing (..)
import String.Extra exposing (..)


simpleLayout : Model -> Html Msg -> Html Msg
simpleLayout model contents =
    div [ class "container" ]
        [ div [ class "columns" ]
            [ div [ class "centered column col-4 col-md-12 col-sm-12" ]
                [ model |> userMsg ]
            ]
        , contents
        ]


sidebarLayout : Model -> Html Msg -> Html Msg -> Html Msg -> Html Msg
sidebarLayout model sidebar nav main =
    div [ class "off-canvas" ]
        [ a [ class "off-canvas-toggle btn btn-primary btn-action", href "#sidebar-id" ]
            [ i [ class "icon icon-menu" ]
                []
            ]
        , div [ class "off-canvas-sidebar p-2", id "sidebar-id" ]
            [ sidebar ]
        , a [ class "off-canvas-overlay", href "#close" ]
            []
        , div [ class "off-canvas-content p-2" ]
            [ nav
            , userMsgPane model
            , main
            ]
        ]


userMsgPane : Model -> Html Msg
userMsgPane model =
    div [ class "centered column col-6 col-md-12 col-sm-12" ]
        [ model |> userMsg ]


userMsg : Model -> Html Msg
userMsg err =
    case err.userMsg of
        Nothing ->
            div [] []

        Just msg ->
            toast msg.contents msg.severity UserMessageRead


navItem : String -> String -> Bool -> Msg -> Html Msg
navItem title icon active action =
    li [ class ("nav-item " ++ activeCss active) ]
        [ a [ href "#", onClick action ]
            [ span []
                []
            , span [ class "mx-1" ]
                [ title |> String.toUpper |> text ]
            ]
        ]


activeCss : Bool -> String
activeCss a =
    case a of
        True ->
            "active"

        False ->
            ""


toast : String -> Severity -> Msg -> Html Msg
toast msg sev action =
    let
        sevStr =
            case sev of
                SevInfo ->
                    "success"

                SevWarn ->
                    "warning"

                SevError ->
                    "error"
    in
    div [ class ("c-hand toast toast-" ++ sevStr), onClick action ]
        [ button [ class "btn btn-clear float-right" ]
            []
        , text msg
        ]


fPassword : String -> String -> (String -> Msg) -> Html Msg
fPassword title v action =
    input [ class "form-input", placeholder title, type_ "password", value v, onInput action ]
        []


fEmail : String -> String -> (String -> Msg) -> Html Msg
fEmail title v action =
    input [ class "form-input", placeholder title, type_ "email", value v, onInput action ]
        []


fLabel : String -> Html Msg
fLabel title =
    label [ class "form-label" ]
        [ text title ]


pButton : String -> Msg -> Html Msg
pButton title action =
    button [ class "btn btn-primary", onClick action ]
        [ text title ]


dButton : String -> Msg -> Html Msg
dButton title action =
    button [ class "btn", type_ "submit", onClick action ]
        [ text title ]


meter : Int -> Int -> Int -> Int -> Int -> Int -> Html Msg
meter high low max min opt v =
    node "meter"
        [ class "meter"
        , attribute "high" (toString high)
        , attribute "low" (toString low)
        , attribute "max" (toString max)
        , attribute "min" (toString min)
        , attribute "optimum" (toString opt)
        , attribute "value" (toString v)
        ]
        []
