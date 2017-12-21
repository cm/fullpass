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
                    case model.node of
                        Nothing ->
                            nodesPane model

                        Just n ->
                            nodePane model n

                Database ->
                    case model.table of
                        Nothing ->
                            tablesPane model

                        Just t ->
                            tablePane t
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


statusCircle : String -> String -> Html Msg
statusCircle size color =
    div
        [ class ("circle bg-" ++ color)
        , style [ ( "width", size ), ( "height", size ) ]
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
            [ view.node |> nodeStatusColor |> statusCircle "28px" ]
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
            [ view |> tableStatusColor |> statusCircle "28px"
            ]
        , td []
            [ button [ class "btn btn-link", onClick (ShowTable view) ]
                [ text view.table.name
                ]

            --    , tableGroups view
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


ips : NodeData -> List (Html Msg)
ips node =
    List.map
        (\i ->
            label [ class "mx-2 label label-rounded", style [ ( "font-size", "14px" ) ] ]
                [ text i ]
        )
        node.info.ips


nodePane : Model -> NodeView -> Html Msg
nodePane model view =
    let
        node =
            view.node
    in
    div [ class "mt-2 container columns" ]
        [ div [ class "col-4" ]
            [ h4 []
                (text node.info.hostname
                    :: ips node
                )
            , node |> nodeResources
            , node |> nodePeers
            , view |> nodeTables model
            ]
        , div [ class "mx-2 col-4" ]
            [ case view.table of
                Nothing ->
                    div [ class "text-gray" ]
                        [ text "No table selected" ]

                Just t ->
                    nodeTablePane1 model view t
            ]
        ]


tableProps : TableData -> List (Html Msg)
tableProps table =
    [ label
        [ class "mx-2 label label-rounded"
        , style [ ( "font-size", "14px" ) ]
        ]
        [ table.kind |> text ]
    , label
        [ class "label label-rounded"
        , style [ ( "font-size", "14px" ) ]
        ]
        [ (toString table.size.count ++ " entries") |> text ]
    ]


tableReplicas : TableData -> Html Msg
tableReplicas table =
    div []
        [ h6 [] [ text "Replicas" ]
        , tableMedia "Memory" table.copies.mem
        , tableMedia "Disc" table.copies.disc
        , tableMedia "Memory and disc" table.copies.both
        ]


tableMedia : String -> List String -> Html Msg
tableMedia title hostnames =
    case hostnames of
        [] ->
            div [] []

        _ ->
            div [ class "container columns" ]
                [ div []
                    [ text title ]
                , div []
                    (List.map
                        (\name ->
                            label [ class "label label-rounded mx-2" ]
                                [ text name ]
                        )
                        hostnames
                    )
                ]


nodeTablePane1 : Model -> NodeView -> TableData -> Html Msg
nodeTablePane1 model view table =
    div []
        [ h4 []
            (text table.name
                :: tableProps table
            )
        , tableReplicas table
        , tableReplica model view table
        ]


onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
    on "change" (Json.map tagger Html.Events.targetValue)


tableReplica : Model -> NodeView -> TableData -> Html Msg
tableReplica model view table =
    case view.state of
        Idle ->
            div [ class "mt-2" ]
                [ tableAddReplicaPane model view table
                , tableDeletePane view table
                ]

        AddingReplica ->
            div [ class "mt-2" ]
                [ div [ class "d-inline-flex" ]
                    [ text "Adding replica..." ]
                , div [ class "mx-2 d-inline-flex loading" ] []
                ]

        DeletingReplica ->
            div [ class "mt-2" ]
                [ div [ class "d-inline-flex" ]
                    [ text "Deleting replica..."
                    ]
                , div [ class "mx-2 d-inline-flex loading" ] []
                ]


tableAddReplicaPane : Model -> NodeView -> TableData -> Html Msg
tableAddReplicaPane model view table =
    let
        peerSelection =
            case view.node.cluster.peers of
                [] ->
                    div [] []

                _ ->
                    select [ class "form-select", onChange HostnameSelected ]
                        (List.map
                            (\name ->
                                option [] [ text name ]
                            )
                            view.node.cluster.peers
                        )

        mediaSelection =
            case table.name of
                "schema" ->
                    div [ class "mt-2" ]
                        [ text "Media: "
                        , span [ class "text-bold" ] [ text "Memory and disc" ]
                        ]

                _ ->
                    select [ class "mt-2 form-select", onChange MediaSelected ]
                        (List.map
                            (\name ->
                                option [] [ text name ]
                            )
                            [ "Disc", "Memory", "Memory and disc" ]
                        )

        buttonStyle =
            case table.name of
                "schema" ->
                    case model.hostname of
                        Nothing ->
                            "disabled"

                        Just _ ->
                            ""

                _ ->
                    case ( model.hostname, model.media ) of
                        ( Just _, Just _ ) ->
                            ""

                        ( _, _ ) ->
                            "disabled"
    in
    case view.node.cluster.peers of
        [] ->
            div [] []

        _ ->
            div []
                [ peerSelection
                , mediaSelection
                , button
                    [ class ("mt-2 btn btn-primary " ++ buttonStyle)
                    , style [ ( "width", "100%" ) ]
                    , onClick (AddTableReplica view table)
                    ]
                    [ text "Add replica" ]
                ]


tableDeletePane : NodeView -> TableData -> Html Msg
tableDeletePane view table =
    let
        title =
            case table.name of
                "schema" ->
                    "Delete entire schema"

                _ ->
                    "Delete this replica"
    in
    div []
        [ button
            [ class "btn btn-error"
            , style [ ( "width", "100%" ) ]
            , onClick (DeleteTableReplica view table)
            ]
            [ text title ]
        ]


nodeResources : NodeData -> Html Msg
nodeResources node =
    div []
        [ h6 [] [ text "Resources" ]
        , node |> cpuUsage
        , node |> memAvailable
        ]


nodePeers : NodeData -> Html Msg
nodePeers node =
    let
        peers =
            case node.cluster.peers of
                [] ->
                    [ text "This server has no connections" ]

                names ->
                    List.map
                        (\name ->
                            label [ class "label label-rounded mx-2" ]
                                [ text name ]
                        )
                        names
    in
    div [ class "mt-2" ]
        [ h6 [] [ text "Connections" ]
        , div []
            [ div [ class "d-inline-flex", style [ ( "vertical-align", "middle" ) ] ]
                [ node |> nodeStatusColor |> statusCircle "16px" ]
            , div [ class "d-inline-flex mx-2", style [ ( "vertical-align", "middle" ) ] ]
                peers
            ]
        ]


nodeTables : Model -> NodeView -> Html Msg
nodeTables model view =
    div [ class "mt-2" ]
        (h6 [] [ text "Database" ]
            :: (view |> dbTables model)
        )


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


tablePane : TableView -> Html Msg
tablePane view =
    div [] []


dbTables : Model -> NodeView -> List (Html Msg)
dbTables model view =
    List.map (dbTable model view) view.node.db.tables


dbTable : Model -> NodeView -> TableData -> Html Msg
dbTable model view table =
    let
        tableView =
            table.name |> tableForName model

        tableColor =
            case tableView of
                Nothing ->
                    "gray"

                Just tv ->
                    tv |> tableStatusColor
    in
    div []
        [ div [ class "d-inline-flex", style [ ( "vertical-align", "middle" ) ] ]
            [ tableColor |> statusCircle "16px" ]
        , div [ class "d-inline-flex mx-2", style [ ( "vertical-align", "middle" ) ] ]
            [ case view.table of
                Just t ->
                    case t.name == table.name of
                        True ->
                            text table.name

                        False ->
                            a [ href "#", onClick (ShowNodeTable view table) ]
                                [ text table.name ]

                _ ->
                    a [ href "#", onClick (ShowNodeTable view table) ]
                        [ text table.name ]
            ]
        ]



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
