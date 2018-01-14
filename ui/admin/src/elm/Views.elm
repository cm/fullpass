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
            case model |> selectedNode of
                Nothing ->
                    "No node to show" |> errorPage model

                Just n ->
                    nodePage model n

        NodeTable ->
            withSelectedNodeTable model
                (\v ->
                    \t ->
                        nodeTablePage model v t
                )

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

        DeletingSchema ->
            "Deleting schema ... "
                |> waitingPage model

        CreatingSchema ->
            "Creating schema ... "
                |> waitingPage model

        DeletingTableReplica ->
            "Deleting table replica ... "
                |> waitingPage model

        CreatingTableReplica ->
            "Creating table replica ... "
                |> waitingPage model

        FetchingTables ->
            "Fetching tables ... "
                |> waitingPage model

        Events ->
            eventsPage model


withSelectedNodeTable : Model -> (NodeView -> TableData -> Html Msg) -> Html Msg
withSelectedNodeTable model next =
    case model |> selectedNode of
        Nothing ->
            "No node to show" |> errorPage model

        Just v ->
            case v |> selectedNodeTable model of
                Nothing ->
                    "No table to show" |> errorPage model

                Just t ->
                    next v t


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


isEventsState : Model -> Bool
isEventsState model =
    case model.state of
        Events ->
            True

        _ ->
            False


sidebar : Model -> Html Msg
sidebar model =
    ul [ class "nav" ]
        [ navItem "Nodes" "icon-drive" (model |> isNodeState) ShowNodes
        , navItem "Tables" "icon-database" (model |> isTablesState) ShowTables
        , navItem "Events" "icon-database" (model |> isEventsState) ShowEvents
        ]


breadcrumb : Model -> Html Msg
breadcrumb model =
    case model.state of
        Nodes ->
            nodesBreadcrumb

        Node ->
            case model |> selectedNode of
                Nothing ->
                    nodesBreadcrumb

                Just v ->
                    nodeBreadcrumb v

        NodeTable ->
            case model |> selectedNode of
                Nothing ->
                    nodesBreadcrumb

                Just v ->
                    nodeTableBreadcrumb model v

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

        Events ->
            eventsBreadcrumb

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


nodeTableBreadcrumb : Model -> NodeView -> Html Msg
nodeTableBreadcrumb model view =
    let
        tableName =
            case model.nodeTable of
                Nothing ->
                    "No table selected !!"

                Just t ->
                    t
    in
    ul [ class "breadcrumb" ]
        [ li [ class "breadcrumb-item" ]
            [ a [ href "#", onClick ShowNodes ]
                [ text "Nodes"
                ]
            ]
        , li [ class "breadcrumb-item" ]
            [ a [ href "#", onClick (ShowNode view.node.info.hostname) ]
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
            [ text view.table.info.name
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
            [ tbody []
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
            [ button [ class "btn btn-link", onClick (ShowNode view.node.info.hostname) ]
                [ text view.node.info.hostname
                ]
            ]
        , td []
            [ nodeDbStateLabel view.node ]
        ]


nodeDbStateLabel : NodeData -> Html Msg
nodeDbStateLabel node =
    case node.db.started of
        True ->
            span [ class "chip mx-2" ]
                [ text "Database online" ]

        False ->
            span [ class "chip mx-2" ]
                [ text "Database offline" ]


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
                            , tableNameSelect (model |> tableNames) NewTableNameChanged
                            ]
                        , div [ class "form-group" ]
                            [ fLabel "Type"
                            , span []
                                [ data.storage |> toString |> text ]
                            ]
                        , div [ class "form-group" ]
                            [ fLabel "Media"
                            , span []
                                [ data.media |> toString |> text ]
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
                            [ dButton "Add replica" AddNewTableReplica ]
                        , div [ class "form-group py-2" ]
                            [ newTableDataReplicasPane model data
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


newTableDataReplicasPane : Model -> NewTableData -> Html Msg
newTableDataReplicasPane model data =
    let
        replicas =
            newTableReplicas data
    in
    case replicas |> List.length of
        0 ->
            div [ class "text-secondary", style [ ( "vertical-align", "middle" ) ] ]
                [ div [ class "d-inline-block" ]
                    [ data |> newTableStatusColor model |> statusCircle "16px" ]
                , div [ class "d-inline-block mx-2 " ]
                    [ "No replicas defined yet" |> text ]
                ]

        _ ->
            table [ class "table table-striped table-hover" ]
                [ thead []
                    [ th []
                        [ data |> newTableStatusColor model |> statusCircle "16px"
                        ]
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
        , td [ class "text-right col-3" ]
            [ dButton "Remove" (RemoveNewTableReplica data) ]
        ]


tablesPage : Model -> Html Msg
tablesPage model =
    div []
        [ div []
            [ pButton "Add new ..." ShowNewTable ]
        , table [ class "mt-2 table table-striped table-hover" ]
            [ tbody []
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
        [ t.table.info.name |> text
        ]
        |> sidebarLayout2 model


nodeTableStatusColor : Model -> TableData -> String
nodeTableStatusColor model t =
    case t |> tableNodes |> List.length of
        1 ->
            "error"

        n ->
            case (model.nodes |> Dict.size) > n of
                True ->
                    "warning"

                False ->
                    "success"


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


newTableStatusColor : Model -> NewTableData -> String
newTableStatusColor model data =
    let
        replicas =
            newTableReplicas data
    in
    case replicas |> List.length of
        0 ->
            "error"

        r ->
            case r >= quorumSize model of
                False ->
                    "warning"

                True ->
                    "success"


tableRow : TableView -> Html Msg
tableRow view =
    let
        tGroups =
            groups view
    in
    case tGroups |> List.length of
        0 ->
            inactiveTableRow view tGroups

        _ ->
            activeTableRow view tGroups


activeTableRow : TableView -> List NodeGroup -> Html Msg
activeTableRow view groups =
    tr []
        [ td [ style [ ( "width", "40px" ) ] ]
            [ view |> tableStatusColor |> statusCircle "28px"
            ]
        , td []
            [ button [ class "btn btn-link", onClick (ShowTable view) ]
                [ text view.table.info.name
                ]
            ]
        , td []
            [ tableGroups groups
            ]
        , td []
            []
        ]


inactiveTableRow : TableView -> List NodeGroup -> Html Msg
inactiveTableRow view groups =
    tr []
        [ td [ style [ ( "width", "40px" ) ] ]
            [ "gray" |> statusCircle "28px"
            ]
        , td []
            [ span [ class "text-secondary" ]
                [ text view.table.info.name ]
            ]
        , td []
            []
        , td []
            [ dButton "Create everywhere" (CreateTableEverywhere view.table) ]
        ]


tableGroups : List NodeGroup -> Html Msg
tableGroups groups =
    div []
        (groups
            |> List.map tableGroup
        )


tableGroup : NodeGroup -> Html Msg
tableGroup group =
    span [ class "chip mx-2" ]
        (group.all |> List.map tableGroupHost)


tableGroupHost : String -> Html Msg
tableGroupHost host =
    span [ class "mx-2 c-hand", onClick (ShowNode host) ]
        [ text host ]


ips : NodeData -> List (Html Msg)
ips node =
    List.map
        (\i ->
            label
                [ class "label label-rounded"
                , style
                    [ ( "margin-right", "4px" )
                    ]
                ]
                [ text i ]
        )
        node.info.ips


nodePage : Model -> NodeView -> Html Msg
nodePage model view =
    let
        node =
            view.node
    in
    div [ class "" ]
        [ div [ class "columns" ]
            [ div [ class "column col-12" ]
                [ h4 []
                    [ text node.info.hostname
                    ]
                ]
            ]
        , div [ class "columns" ]
            [ div [ class "column col-3" ]
                [ text "Addresses" ]
            , div [ class "column col-9" ]
                (ips node)
            ]
        , div [ class "columns mt-2" ]
            [ div [ class "column col-3" ]
                [ text "Performance" ]
            , div [ class "column col-9" ]
                [ node |> nodeResources ]
            ]
        , div [ class "columns mt-2" ]
            [ div [ class "column col-3" ]
                [ text "Peers" ]
            , div [ class "column col-9" ]
                [ node |> nodePeers ]
            ]
        , div [ class "columns mt-2" ]
            [ div [ class "column col-3" ]
                [ text "Database" ]
            , div [ class "column col-9" ]
                [ node |> nodeDbSwitch ]
            ]
        , div [ class "columns mt-2" ]
            [ div [ class "column col-3" ]
                [ text "Schema" ]
            , div [ class "column col-9" ]
                [ view |> nodeSchema model ]
            ]
        , div [ class "columns mt-2" ]
            [ div [ class "column col-3" ]
                [ text "Tables" ]
            , div [ class "column col-9" ]
                [ view |> nodeTables model ]
            ]
        ]
        |> sidebarLayout2 model


tableProps : TableData -> List (Html Msg)
tableProps table =
    [ label
        [ class "mx-2 label label-rounded"
        , style [ ( "font-size", "14px" ) ]
        ]
        [ table.info.kind |> text ]
    , label
        [ class "label label-rounded"
        , style [ ( "font-size", "14px" ) ]
        ]
        [ (toString table.size.count ++ " entries") |> text ]
    ]


tableReplicas : TableData -> Html Msg
tableReplicas table =
    div []
        [ tableMediaPane "Memory" table.copies.mem table
        , tableMediaPane "Disc" table.copies.disc table
        , tableMediaPane "Memory and disc" table.copies.both table
        ]


tableMediaPane : String -> List String -> TableData -> Html Msg
tableMediaPane title hostnames t =
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
                            span [ class "chip mx-2" ]
                                [ span [] [ text name ]

                                --, a
                                --    [ class "btn btn-clear"
                                --    , attribute "aria-label" "Close"
                                --    , attribute "role" "button"
                                --    , onClick (DeleteTableReplica name t.name)
                                --    ]
                                --    []
                                ]
                        )
                        hostnames
                    )
                ]


nodeTablePage : Model -> NodeView -> TableData -> Html Msg
nodeTablePage model view table =
    div []
        [ h4 []
            [ text table.info.name
            ]
        , div [ class "columns mt-2" ]
            [ div [ class "column col-3" ]
                [ text "Type" ]
            , div [ class "column col-9" ]
                [ case table.info.kind |> toTableStorage of
                    Nothing ->
                        "Unknown" |> text

                    Just s ->
                        s |> tableStorageToString |> text
                ]
            ]
        , div [ class "columns mt-2" ]
            [ div [ class "column col-3" ]
                [ text "Size" ]
            , div [ class "column col-9" ]
                [ (toString table.size.count
                    ++ " entries, "
                    ++ (table.size.words |> wordsToBytes |> toString)
                    ++ " bytes"
                  )
                    |> text
                ]
            ]
        , div [ class "columns mt-2" ]
            [ div [ class "column col-3" ]
                [ text "Replicas" ]
            , div [ class "column col-9" ]
                [ tableReplicas table
                ]
            ]
        , div [ class "columns mt-2" ]
            [ div [ class "column col-3" ]
                []
            , div [ class "column col-9" ]
                [ tableAddReplicaPane model view table ]
            ]
        , div [ class "columns mt-2" ]
            [ div [ class "column col-3" ]
                []
            , div [ class "column col-9" ]
                [ tableReplicaDeleteButton view table ]
            ]
        ]
        |> sidebarLayout2 model


onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
    on "change" (Json.map tagger Html.Events.targetValue)


tableNameSelect : List String -> (String -> Msg) -> Html Msg
tableNameSelect names action =
    select [ class "form-select", onChange action ]
        (List.map
            (\s ->
                option [] [ s |> text ]
            )
            names
        )


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
            case table.info.name of
                "schema" ->
                    div [ class "mt-2" ]
                        [ text "Media: "
                        , span [ class "text-bold" ] [ text "Memory and disc" ]
                        ]

                _ ->
                    tableMediaSelect model MediaSelected

        buttonStyle =
            case table.info.name of
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
            span [ class "text-secondary" ]
                [ text "Not enough peers node available for new replicas"
                ]

        _ ->
            div []
                [ peerSelection
                , mediaSelection
                , button
                    [ class ("mt-2 btn btn-primary " ++ buttonStyle)
                    , onClick (CreateTableReplica view table)
                    ]
                    [ text "Add replica" ]
                ]


tableReplicaDeleteButton : NodeView -> TableData -> Html Msg
tableReplicaDeleteButton view table =
    case table.info.name of
        "schema" ->
            div [] []

        _ ->
            div []
                [ button
                    [ class "btn btn-error"
                    , onClick (DeleteTableReplica view.node.info.hostname table.info.name)
                    ]
                    [ text "Delete this replica" ]
                ]


nodeResources : NodeData -> Html Msg
nodeResources node =
    div []
        [ node |> cpuUsage
        , node |> memAvailable
        ]


nodeSchema : Model -> NodeView -> Html Msg
nodeSchema model view =
    case view.node.db.started of
        True ->
            div []
                [ nodeSchemaReplicas model view
                , nodeSchemaActions view
                ]

        False ->
            span [ class "text-secondary" ]
                [ text "Database is stopped" ]


nodeSchemaReplicas : Model -> NodeView -> Html Msg
nodeSchemaReplicas model view =
    case nodeTable "schema" view.node of
        Nothing ->
            span [ class "text-secondary" ] [ text "No schema information available" ]

        Just t ->
            let
                replicas =
                    case t |> tableNodes of
                        [] ->
                            span [ class "text-secondary" ] [ text "No replicas found" ]

                        hosts ->
                            div []
                                (hosts
                                    |> List.map
                                        (\host ->
                                            label
                                                [ class "mx-2 c-hand label label-rounded"
                                                , onClick (ShowNode host)
                                                ]
                                                [ text host ]
                                        )
                                )
            in
            div [ class "mt-2" ]
                [ div []
                    [ div [ class "mx-2 d-inline-flex", style [ ( "vertical-align", "middle" ) ] ]
                        [ t |> nodeTableStatusColor model |> statusCircle "16px" ]
                    , div [ class "d-inline-flex mx-2", style [ ( "vertical-align", "middle" ) ] ]
                        [ replicas ]
                    ]
                ]


nodeSchemaActions : NodeView -> Html Msg
nodeSchemaActions view =
    div [ class "mt-2" ]
        [ case hasEmptySchema view.node of
            True ->
                pButton "Create schema" (CreateSchema view)

            False ->
                div []
                    [ eButton "Delete schema" (DeleteSchema view)
                    , span [ class "mx-1" ] []
                    , dButton "Edit schema ..." (ShowNodeTable view "schema")
                    ]
        ]


nodeDbSwitch : NodeData -> Html Msg
nodeDbSwitch node =
    fSwitch "" node.db.started (ToggleNodeDb node)


nodePeers : NodeData -> Html Msg
nodePeers node =
    let
        peers =
            case node.cluster.peers of
                [] ->
                    [ span [ class "mx-2" ]
                        [ text "This server has no connections" ]
                    ]

                names ->
                    List.map
                        (\name ->
                            label [ class "label label-rounded mx-2" ]
                                [ text name ]
                        )
                        names
    in
    div [ class "mt-2" ]
        [ div []
            [ div [ class "mx-2 d-inline-flex", style [ ( "vertical-align", "middle" ) ] ]
                [ node |> nodeStatusColor |> statusCircle "16px" ]
            , div [ class "d-inline-flex mx-2", style [ ( "vertical-align", "middle" ) ] ]
                peers
            ]
        ]


nodeTables : Model -> NodeView -> Html Msg
nodeTables model view =
    let
        userTables =
            view.node.db.tables
                |> List.filter
                    (\t ->
                        t.info.name /= "schema"
                    )
    in
    case view.node.db.started of
        False ->
            span [ class "text-secondary" ]
                [ text "Database is stopped" ]

        True ->
            case userTables of
                [] ->
                    span [] [ text "No tables yet" ]

                _ ->
                    table [ class "mt-2 table table-striped table-hover" ]
                        [ tbody []
                            (List.map
                                (dbTable model view)
                                userTables
                            )
                        ]


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


dbTable : Model -> NodeView -> TableData -> Html Msg
dbTable model view table =
    let
        tableView =
            table.info.name |> tableForName model

        tableColor =
            case tableView of
                Nothing ->
                    "gray"

                Just tv ->
                    tv |> tableStatusColor
    in
    tr []
        [ td [ class "col-1" ]
            [ tableColor |> statusCircle "16px"
            ]
        , td []
            [ a [ href "#", onClick (ShowNodeTable view table.info.name) ]
                [ text table.info.name ]
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
            toast msg.contents msg.severity UserMessageRead


activeCss : Bool -> String
activeCss a =
    case a of
        True ->
            "active"

        False ->
            ""


eventsPage : Model -> Html Msg
eventsPage model =
    div [ class "" ]
        [ table [ class "table table-striped table-hover" ]
            [ tbody []
                (model
                    |> events
                    |> List.map eventRow
                )
            ]
        ]
        |> sidebarLayout2 model


eventsBreadcrumb : Html Msg
eventsBreadcrumb =
    ul [ class "breadcrumb" ]
        [ li [ class "breadcrumb-item" ]
            [ text "Events"
            ]
        ]


eventRow : EventData -> Html Msg
eventRow ev =
    tr []
        [ td []
            [ text ev.kind
            ]
        , td []
            [ text ev.info
            ]
        , td []
            [ ev.date |> humanDate |> text ]
        ]


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


fSwitch : String -> Bool -> Msg -> Html Msg
fSwitch title isChecked action =
    label [ class "form-switch", onClick action ]
        [ input [ type_ "checkbox", checked isChecked ]
            []
        , i [ class "form-icon", onClick action ]
            []
        , text title
        ]


pButton : String -> Msg -> Html Msg
pButton title action =
    button [ class "btn btn-primary", onClick action ]
        [ text title ]


eButton : String -> Msg -> Html Msg
eButton title action =
    button [ class "btn btn-error", onClick action ]
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
