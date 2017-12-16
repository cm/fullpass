module Views exposing (..)

import Dict exposing (..)
import FileReader exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Init exposing (..)
import Json.Decode as Json
import Messages exposing (..)
import Models exposing (..)
import String.Extra exposing (..)
import UIKit exposing (..)


view : Model -> Html Msg
view model =
    case model.state of
        SignedOut ->
            loginPage model

        SignedIn ->
            homePage model


loginPage : Model -> Html Msg
loginPage model =
    div [ class "columns" ]
        [ div [ class "column col-4 col-md-6 col-sm-12" ]
            [ div
                [ class "form-horizontal" ]
                [ div [ class "form-group" ]
                    [ div [ class "col-12" ]
                        [ fLabel "Email"
                        , fEmail "Email address" model.loginData.email OnLoginEmail
                        ]
                    ]
                , div [ class "form-group" ]
                    [ div [ class "col-12" ]
                        [ fLabel "Password"
                        , fPassword "Password" model.loginData.password OnLoginPassword
                        ]
                    ]
                , div [ class "form-group" ]
                    [ div [ class "col-12" ]
                        [ pButton "Login" Login
                        ]
                    ]
                ]
            ]
        ]
        |> simpleLayout model


homePage : Model -> Html Msg
homePage model =
    let
        s =
            sidebar model

        n =
            nav model

        main =
            case model.perspective of
                Servers ->
                    nodesPane model

                Database ->
                    tablesPane model
    in
    sidebarLayout model s n main


nav : Model -> Html Msg
nav model =
    header [ class "navbar" ]
        [ section [ class "navbar-section" ]
            [ breadcrumb model ]
        , section [ class "navbar-center" ]
            []
        , section [ class "navbar-section" ]
            [ dButton "Logout" Logout
            ]
        ]


sidebar : Model -> Html Msg
sidebar model =
    ul [ class "nav" ]
        [ navItem "Servers" "icon-drive" (model.perspective == Servers) (ShowPerspective Servers)
        , navItem "Database" "icon-database" (model.perspective == Database) (ShowPerspective Database)
        ]


breadcrumb : Model -> Html Msg
breadcrumb model =
    case model.perspective of
        Servers ->
            case model.node of
                Nothing ->
                    nodesBreadcrumb

                Just v ->
                    nodeBreadcrumb v

        Database ->
            case model.table of
                Nothing ->
                    tablesBreadcrumb

                Just t ->
                    tableBreadcrumb t


nodesBreadcrumb : Html Msg
nodesBreadcrumb =
    ul [ class "breadcrumb" ]
        [ li [ class "breadcrumb-item" ]
            [ text "Servers"
            ]
        ]


nodeBreadcrumb : NodeView -> Html Msg
nodeBreadcrumb view =
    ul [ class "breadcrumb" ]
        [ li [ class "breadcrumb-item" ]
            [ a [ href "#", onClick (ShowPerspective Servers) ]
                [ text "Servers"
                ]
            ]
        , li [ class "breadcrumb-item" ]
            [ text view.node.info.hostname
            ]
        ]


tablesBreadcrumb : Html Msg
tablesBreadcrumb =
    ul [ class "breadcrumb" ]
        [ li [ class "breadcrumb-item" ]
            [ text "Database"
            ]
        ]


tableBreadcrumb : TableView -> Html Msg
tableBreadcrumb view =
    ul [ class "breadcrumb" ]
        [ li [ class "breadcrumb-item" ]
            [ a [ href "#", onClick (ShowPerspective Database) ]
                [ text "Database" ]
            ]
        , li [ class "breadcrumb-item" ]
            [ text view.table.name
            ]
        ]


nodesPane : Model -> Html Msg
nodesPane model =
    div [ class "" ]
        [ table [ class "table table-striped table-hover" ]
            [ thead []
                [ tr []
                    [ th [ style [ ( "width", "40px" ) ] ] []
                    , th [] [ text "Name" ]
                    ]
                ]
            , tbody []
                (model
                    |> nodes
                    |> List.map nodeRow
                )
            ]
        ]


statusCircle : String -> Html Msg
statusCircle color =
    div
        [ class ("circle bg-" ++ color)
        , style [ ( "width", "28px" ), ( "height", "28px" ) ]
        ]
        []


nodeStatusColor : NodeData -> String
nodeStatusColor n =
    case n.cluster.health of
        "green" ->
            "success"

        "yellow" ->
            "warning"

        _ ->
            "error"


nodeRow : NodeView -> Html Msg
nodeRow view =
    tr []
        [ td [ style [ ( "width", "40px" ) ] ]
            [ view.node |> nodeStatusColor |> statusCircle ]
        , td []
            [ button [ class "btn btn-link", onClick (ShowNode view) ]
                [ text view.node.info.hostname
                ]
            ]
        ]


tablesPane : Model -> Html Msg
tablesPane model =
    div [ class "" ]
        [ table [ class "table table-striped table-hover" ]
            [ thead []
                [ tr []
                    [ th [ style [ ( "width", "40px" ) ] ] []
                    , th [] [ text "Name" ]
                    ]
                ]
            , tbody []
                (model
                    |> tables
                    |> List.map tableRow
                )
            ]
        ]


tableStatusColor : TableView -> String
tableStatusColor v =
    let
        grps =
            v |> groups
    in
    case grps |> List.length of
        1 ->
            case grps |> List.head of
                Nothing ->
                    "error"

                Just g ->
                    case g.all |> List.length of
                        1 ->
                            "warning"

                        _ ->
                            "success"

        _ ->
            "error"


tableRow : TableView -> Html Msg
tableRow view =
    tr []
        [ td [ style [ ( "width", "40px" ) ] ]
            [ view |> tableStatusColor |> statusCircle
            ]
        , td []
            [ button [ class "btn btn-link", onClick (ShowTable view) ]
                [ text view.table.name
                ]
            , tableGroups view
            ]
        ]


tableGroups : TableView -> Html Msg
tableGroups view =
    div []
        (groups view
            |> List.map tableGroup
        )


tableGroup : NodeGroup -> Html Msg
tableGroup group =
    label [ class "label label-rounded mx-2" ]
        [ group.all |> String.join " " |> text ]


nodePane : NodeView -> Html Msg
nodePane view =
    let
        node =
            view.node
    in
    div [ class "panel col-12 col-sm-12 col-md-12" ]
        [ div [ class "panel-header text-center" ]
            [ div [ class "panel-title m-10" ]
                [ text node.info.hostname ]
            , div [ class "panel-subtitle" ]
                [ small []
                    [ node.info.ips |> String.join " " |> text
                    ]
                , br [] []
                , node |> healthSummary
                ]
            ]
        , div [ class "panel-body" ]
            [ node |> cpuUsage
            , node |> memAvailable
            , view |> nodeTabs
            , view |> nodeContents

            --, node |> dbSummary
            --, node |> dbInfo
            --high low max min opt v
            ]
        , div [ class "panel-footer" ]
            [--button [ class "btn btn-block btn-primary" ] [ text "Open" ]
            ]
        ]


nodeTabs : NodeView -> Html Msg
nodeTabs view =
    ul [ class "tab tab-block" ]
        [ li [ class ("tab-item " ++ activeCss (view.tab == Connections)) ]
            [ a [ href "#", onClick (ShowNodeTab view Connections) ]
                [ text "Peers" ]
            ]
        , li [ class ("tab-item " ++ activeCss (view.tab == Tables)) ]
            [ a [ href "#", onClick (ShowNodeTab view Tables) ]
                [ text "Tables" ]
            ]
        ]


nodeContents : NodeView -> Html Msg
nodeContents view =
    case view.tab of
        Connections ->
            div []
                (view.node |> peers)

        Tables ->
            div []
                (view.node |> dbTables)


healthSummary : NodeData -> Html Msg
healthSummary node =
    let
        labelStyle =
            case node.cluster.health of
                "green" ->
                    "success"

                "yellow" ->
                    "warning"

                "red" ->
                    "error"

                _ ->
                    "default"
    in
    span [ class ("mt-2 label label-" ++ labelStyle) ] [ node.cluster.health |> String.toUpper |> text ]


dbInfo : NodeData -> Html Msg
dbInfo node =
    div [ class "accordion mt-2" ]
        [ input [ attribute "d" "", attribute "hidden" "", id "accordion-1", name "accordion-checkbox", type_ "checkbox" ]
            []
        , label [ class "c-hand", for "accordion-1" ]
            [ node |> dbSummary
            ]
        , div [ class "accordion-body" ]
            [ div []
                (node |> dbTables)
            ]
        ]


dbTables : NodeData -> List (Html Msg)
dbTables node =
    List.map dbTable node.db.tables


dbTable : TableData -> Html Msg
dbTable table =
    div []
        [ text table.name ]


peers : NodeData -> List (Html Msg)
peers node =
    List.map peer node.cluster.peers


peer : String -> Html Msg
peer name =
    div []
        [ text name ]



--details [ class "accordion", attribute "open" "" ]
--    [ summary [ class "c-hand" ]
--        [ node |> dbSummary ]
--    , div [ class "accordion-body" ]
--        [ text "Database detail " ]
--    ]


dbSummary : NodeData -> Html Msg
dbSummary node =
    div [ class "tile tile-centered" ]
        [ div [ class "tile-content" ]
            [ div [ class "tile-title" ]
                [ text "Database" ]
            ]
        , div [ class "tile-action" ]
            [ toggleDisplay node.db.started ]
        ]


toggleDisplay : Bool -> Html Msg
toggleDisplay v =
    case v of
        False ->
            span [ class "label" ] [ text "OFF" ]

        True ->
            span [ class "label label-primary" ] [ text "ON" ]


cpuUsage : NodeData -> Html Msg
cpuUsage node =
    let
        nodeCpu =
            node |> cpu
    in
    meter ("CPU: " ++ toString nodeCpu ++ "%") 80 50 100 0 0 nodeCpu


memAvailable : NodeData -> Html Msg
memAvailable node =
    let
        nodeMem =
            node |> mem
    in
    meter ("RAM: " ++ toString nodeMem ++ "%") 80 20 100 0 100 nodeMem



--
-- UIKit start here
--
--


simpleLayout : Model -> Html Msg -> Html Msg
simpleLayout model contents =
    div [ class "container" ]
        [ div [ class "columns" ]
            [ div [ class "centered column col-4 col-md-12 col-sm-12" ]
                [ model |> userMsg ]
            ]
        , contents
        ]


navLayout : Model -> Html Msg -> Html Msg
navLayout model contents =
    div [ class "container mt-2" ]
        [ nav model
        , div [ class "centered mt-2 col-4 col-md-12 col-sm-12" ]
            [ model |> userMsg ]
        , div [ class "mt-2" ]
            [ contents ]
        ]



--sidebarLayout : Model -> Html Msg -> Html Msg -> Html Msg -> Html Msg
--sidebarLayout model sidebar nav main =
--    div [ class "off-canvas" ]
--        [ a [ class "off-canvas-toggle btn btn-primary btn-action", href "#sidebar-id" ]
--            [ i [ class "icon icon-menu" ]
--                []
--            ]
--        , div [ class "off-canvas-sidebar p-2", id "sidebar-id" ]
--            [ sidebar ]
--        , a [ class "off-canvas-overlay", href "#close" ]
--            []
--        , div [ class "off-canvas-content p-2" ]
--            [ nav
--            , userMsgPane model
--            , main
--            ]
--        ]


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
            toast msg.contents msg.severity


activeCss : Bool -> String
activeCss a =
    case a of
        True ->
            "active"

        False ->
            ""


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


meter : String -> Int -> Int -> Int -> Int -> Int -> Int -> Html Msg
meter label high low max min opt v =
    div [ class "mt-2" ]
        [ small [] [ text label ]
        , node "meter"
            [ class "meter tooltip"
            , attribute "high" (toString high)
            , attribute "low" (toString low)
            , attribute "max" (toString max)
            , attribute "min" (toString min)
            , attribute "optimum" (toString opt)
            , attribute "value" (toString v)
            , attribute "data-tooltip" label
            ]
            []
        ]
