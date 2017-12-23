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

        Nodes ->
            nodesPage model

        Node ->
            case model.node of
                Nothing ->
                    "No node to show" |> errorPage model

                Just n ->
                    nodePage model n

        NodeTable ->
            case model.node of
                Nothing ->
                    "No node to show" |> errorPage model

                Just v ->
                    case v.table of
                        Nothing ->
                            "No table to show" |> errorPage model

                        Just t ->
                            nodePage model v

        Tables ->
            tablesPage model

        Table ->
            case model.table of
                Nothing ->
                    "No page to show" |> errorPage model

                Just t ->
                    tablePage model t

        NewTable ->
            newTablePage model

        CreatingTable ->
            "Creating page ... "
                |> waitingPage model


errorPage : Model -> String -> Html Msg
errorPage model msg =
    div [] [ text msg ]
        |> sidebarLayout2 model


sidebarLayout2 : Model -> Html Msg -> Html Msg
sidebarLayout2 model main =
    let
        s =
            sidebar model

        n =
            nav model
    in
    sidebarLayout model s n main


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


isNodeState : Model -> Bool
isNodeState model =
    case model.state of
        Nodes ->
            True

        Node ->
            True

        NodeTable ->
            True

        _ ->
            False


isTablesState : Model -> Bool
isTablesState model =
    case model.state of
        Tables ->
            True

        Table ->
            True

        NewTable ->
            True

        _ ->
            False


sidebar : Model -> Html Msg
sidebar model =
    ul [ class "nav" ]
        [ navItem "Nodes" "icon-drive" (model |> isNodeState) ShowNodes
        , navItem "Tables" "icon-database" (model |> isTablesState) ShowTables
        ]


breadcrumb : Model -> Html Msg
breadcrumb model =
    case model.state of
        Nodes ->
            nodesBreadcrumb

        Node ->
            case model.node of
                Nothing ->
                    nodesBreadcrumb

                Just v ->
                    nodeBreadcrumb v

        NodeTable ->
            case model.node of
                Nothing ->
                    nodesBreadcrumb

                Just v ->
                    nodeTableBreadcrumb v

        Tables ->
            tablesBreadcrumb

        Table ->
            case model.table of
                Nothing ->
                    tablesBreadcrumb

                Just t ->
                    tableBreadcrumb t

        NewTable ->
            newTableBreadcrumb

        _ ->
            div [] []


nodesBreadcrumb : Html Msg
nodesBreadcrumb =
    ul [ class "breadcrumb" ]
        [ li [ class "breadcrumb-item" ]
            [ text "Nodes"
            ]
        ]


nodeBreadcrumb : NodeView -> Html Msg
nodeBreadcrumb view =
    ul [ class "breadcrumb" ]
        [ li [ class "breadcrumb-item" ]
            [ a [ href "#", onClick ShowNodes ]
                [ text "Nodes"
                ]
            ]
        , li [ class "breadcrumb-item" ]
            [ text view.node.info.hostname
            ]
        ]


nodeTableBreadcrumb : NodeView -> Html Msg
nodeTableBreadcrumb view =
    let
        tableName =
            case view.table of
                Nothing ->
                    "Unknown table"

                Just t ->
                    t.name
    in
    ul [ class "breadcrumb" ]
        [ li [ class "breadcrumb-item" ]
            [ a [ href "#", onClick ShowNodes ]
                [ text "Nodes"
                ]
            ]
        , li [ class "breadcrumb-item" ]
            [ a [ href "#", onClick (ShowNode view) ]
                [ text view.node.info.hostname ]
            ]
        , li [ class "breadcrumb-item" ]
            [ text tableName
            ]
        ]


tablesBreadcrumb : Html Msg
tablesBreadcrumb =
    ul [ class "breadcrumb" ]
        [ li [ class "breadcrumb-item" ]
            [ text "Tables"
            ]
        ]


tableBreadcrumb : TableView -> Html Msg
tableBreadcrumb view =
    ul [ class "breadcrumb" ]
        [ li [ class "breadcrumb-item" ]
            [ a [ href "#", onClick ShowTables ]
                [ text "Tables" ]
            ]
        , li [ class "breadcrumb-item" ]
            [ text view.table.name
            ]
        ]


newTableBreadcrumb : Html Msg
newTableBreadcrumb =
    ul [ class "breadcrumb" ]
        [ li [ class "breadcrumb-item" ]
            [ a [ href "#", onClick ShowTables ]
                [ text "Tables" ]
            ]
        , li [ class "breadcrumb-item" ]
            [ text "Add new ..."
            ]
        ]


nodesPage : Model -> Html Msg
nodesPage model =
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
        |> sidebarLayout2 model


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


waitingPage : Model -> String -> Html Msg
waitingPage model msg =
    div []
        [ msg |> text ]
        |> sidebarLayout2 model


newTablePage : Model -> Html Msg
newTablePage model =
    case model.newTableData of
        Nothing ->
            "No new table data was initialized. This is a bug, please fix"
                |> errorPage model

        Just data ->
            div [ class "" ]
                [ div [ class "" ]
                    [ div [ class "form-group" ]
                        [ h4 [] [ text "Add table" ] ]
                    ]
                , div [ class "columns" ]
                    [ div [ class "column col-6" ]
                        [ div [ class "form-group" ]
                            [ fLabel "Table name"
                            , fText "Table name" data.name NewTableNameChanged
                            ]
                        , div [ class "form-group" ]
                            [ fLabel "Type"
                            , tableStorageSelect data.storage NewTableStorageChanged
                            ]
                        ]
                    ]
                , div [ class "mt-2 columns " ]
                    [ div [ class "column col-6" ]
                        [ div [ class "form-group" ]
                            [ fLabel "Replicas"
                            ]
                        , div [ class "form-group" ]
                            [ nodesSelect model NewTableReplicaNodeChanged
                            ]
                        , div [ class "form-group" ]
                            [ tableMediaSelect model NewTableReplicaMediaChanged
                            ]
                        , div [ class "form-group" ]
                            [ dButton "Add replica" AddNewTableReplica ]
                        , div [ class "form-group py-2" ]
                            [ newTableDataReplicasPane data
                            ]
                        ]
                    ]
                , div [ class "columns mt-2" ]
                    [ div [ class "column col-2" ]
                        [ case model |> canCreateTable of
                            True ->
                                pButton "Create table" CreateTable

                            False ->
                                disabledPButton "Create table"
                        ]
                    ]
                ]
                |> sidebarLayout2 model


newTableDataReplicasPane : NewTableData -> Html Msg
newTableDataReplicasPane data =
    let
        replicas =
            newTableReplicas data
    in
    case replicas |> List.length of
        0 ->
            div [ class "text-secondary", style [ ( "vertical-align", "middle" ) ] ]
                [ div [ class "d-inline-block" ]
                    [ data |> newTableStatusColor |> statusCircle "16px" ]
                , div [ class "d-inline-block mx-2 " ]
                    [ "No replicas defined yet" |> text ]
                ]

        _ ->
            table [ class "table table-striped table-hover" ]
                [ thead []
                    [ th []
                        [ data |> newTableStatusColor |> statusCircle "16px"
                        ]
                    , th [] []
                    , th [] []
                    ]
                , tbody []
                    (List.map newTableDataReplica replicas)
                ]


newTableDataReplica : TableReplicaData -> Html Msg
newTableDataReplica data =
    tr []
        [ td [ class "col-3" ]
            [ text data.node ]
        , td [ class "col-6" ]
            [ data.media |> tableMediaToString |> text ]
        , td [ class "text-right col-3" ]
            [ dButton "Remove" (RemoveNewTableReplica data) ]
        ]


tablesPage : Model -> Html Msg
tablesPage model =
    div []
        [ div []
            [ pButton "Add new..." ShowNewTable ]
        , table [ class "mt-2 table table-striped table-hover" ]
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
        |> sidebarLayout2 model


tablePage : Model -> TableView -> Html Msg
tablePage model t =
    div []
        [ t.table.name |> text
        ]
        |> sidebarLayout2 model


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


newTableStatusColor : NewTableData -> String
newTableStatusColor data =
    let
        replicas =
            newTableReplicas data
    in
    case replicas |> List.length of
        0 ->
            "error"

        1 ->
            "warning"

        _ ->
            "success"


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


nodePage : Model -> NodeView -> Html Msg
nodePage model view =
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
        |> sidebarLayout2 model


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
        , tableMediaPane "Memory" table.copies.mem
        , tableMediaPane "Disc" table.copies.disc
        , tableMediaPane "Memory and disc" table.copies.both
        ]


tableMediaPane : String -> List String -> Html Msg
tableMediaPane title hostnames =
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


tableStorageSelect : TableStorage -> (String -> Msg) -> Html Msg
tableStorageSelect selected action =
    select [ class "form-select", onChange action ]
        (List.map
            (\s ->
                option [] [ s |> tableStorageToString |> text ]
            )
            tableStorage
        )


tableMediaSelect : Model -> (String -> Msg) -> Html Msg
tableMediaSelect model action =
    select [ class "form-select", onChange action ]
        (List.map
            (\m ->
                option [] [ m |> tableMediaToString |> text ]
            )
            tableMedia
        )


nodesSelect : Model -> (String -> Msg) -> Html Msg
nodesSelect model action =
    select [ class "form-select", onChange action ]
        (List.map
            (\v ->
                option [] [ text v.node.info.hostname ]
            )
            (nodes model)
        )


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


fText : String -> String -> (String -> Msg) -> Html Msg
fText title v action =
    input [ class "form-input", placeholder title, type_ "text", value v, onInput action ]
        []


fLabel : String -> Html Msg
fLabel title =
    label [ class "form-label" ]
        [ text title ]


pButton : String -> Msg -> Html Msg
pButton title action =
    button [ class "btn btn-primary", onClick action ]
        [ text title ]


disabledPButton : String -> Html Msg
disabledPButton title =
    button [ class "btn btn-primary disabled" ]
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
