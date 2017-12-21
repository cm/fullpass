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
    , loginData = newLoginData
    , userMsg = Nothing
    , session = ""
    , user = Nothing
    , state = SignedOut
    , nodes = Dict.empty
    , tables = Dict.empty
    , perspective = Servers
    , node = Nothing
    , table = Nothing
    , hostname = Nothing
    , media = Nothing
    }


loginDataWithPassword : LoginData -> String -> LoginData
loginDataWithPassword data v =
    { data | password = v }


loginDataWithEmail : LoginData -> String -> LoginData
loginDataWithEmail data v =
    { data | email = v }


modelWithSession : Flags -> String -> Model
modelWithSession flags sid =
    let
        model =
            newModel flags
    in
    { model | session = sid }


severityStr : Severity -> String
severityStr sev =
    case sev of
        SevInfo ->
            "success"

        SevWarn ->
            "warning"

        SevError ->
            "error"


stringAsError : String -> UserMsg
stringAsError reason =
    { contents = reason
    , severity = SevError
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
    { email = ""
    , password = ""
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    newModel flags ! []


nodeView : NodeData -> NodeView
nodeView node =
    { node = node
    , table = Nothing
    , state = Idle
    }


viewWithNode : NodeView -> NodeData -> NodeView
viewWithNode v n =
    { v | node = n }


indexedNodes : Model -> List NodeData -> Dict String NodeView
indexedNodes model nodes =
    List.foldl
        (\n ->
            \i ->
                let
                    view =
                        case Dict.get n.info.hostname model.nodes of
                            Nothing ->
                                nodeView n

                            Just v ->
                                viewWithNode v n
                in
                Dict.insert n.info.hostname view i
        )
        Dict.empty
        nodes


modelWithNodeView : Model -> NodeView -> Model
modelWithNodeView model view =
    let
        i2 =
            Dict.insert view.node.info.hostname view model.nodes
    in
    { model | nodes = i2 }


indexedTables : Model -> List NodeData -> Dict String TableView
indexedTables model nodes =
    List.foldl
        (\n ->
            \i ->
                List.foldl
                    (\t ->
                        \j ->
                            let
                                view =
                                    case Dict.get t.name j of
                                        Nothing ->
                                            tableView t n

                                        Just v ->
                                            tableViewWithNode v t n
                            in
                            Dict.insert t.name view j
                    )
                    i
                    n.db.tables
        )
        Dict.empty
        nodes


nodeTable : String -> NodeData -> Maybe TableData
nodeTable name node =
    node.db.tables
        |> List.filter
            (\t ->
                t.name == name
            )
        |> List.head


tableGroup : TableData -> NodeData -> Maybe NodeGroup
tableGroup table node =
    case nodeTable table.name node of
        Nothing ->
            Nothing

        Just t ->
            let
                all =
                    (table.copies.both
                        ++ table.copies.mem
                        ++ table.copies.disc
                    )
                        |> List.sort

                name =
                    all |> String.join "-"
            in
            Just
                { name = name
                , all = all
                , mem = table.copies.mem
                , disc = table.copies.disc
                , both = table.copies.both
                }


tableView : TableData -> NodeData -> TableView
tableView t n =
    let
        groups =
            case tableGroup t n of
                Nothing ->
                    Dict.empty

                Just g ->
                    Dict.insert g.name g Dict.empty
    in
    { table = t
    , groups = groups
    }


tableViewWithNode : TableView -> TableData -> NodeData -> TableView
tableViewWithNode view t n =
    let
        groups2 =
            case tableGroup t n of
                Nothing ->
                    view.groups

                Just g ->
                    Dict.insert g.name g view.groups
    in
    { view | groups = groups2 }


groups : TableView -> List NodeGroup
groups v =
    v.groups
        |> Dict.values


nodeByName : NodeView -> NodeView -> Order
nodeByName n1 n2 =
    compare n1.node.info.hostname n2.node.info.hostname


tableByName : TableView -> TableView -> Order
tableByName n1 n2 =
    compare n1.table.name n2.table.name


nodes : Model -> List NodeView
nodes model =
    model.nodes
        |> Dict.values
        |> List.sortWith nodeByName


tables : Model -> List TableView
tables model =
    model.tables
        |> Dict.values
        |> List.sortWith tableByName


tableForName : Model -> String -> Maybe TableView
tableForName model name =
    Dict.get name model.tables


replicas : TableView -> List Int
replicas view =
    view.groups
        |> Dict.values
        |> List.foldl
            (\g ->
                \r ->
                    (g.all |> List.length) :: r
            )
            []


avg : List Float -> Float
avg list =
    (list |> List.sum) / (list |> List.length |> toFloat)


mem : NodeData -> Int
mem node =
    (toFloat node.perf.mem.free
        / toFloat node.perf.mem.total
        * 100
    )
        |> round


cpu : NodeData -> Int
cpu node =
    node.perf.cpu
        |> List.map
            (\cpu ->
                cpu.busy / (cpu.busy + cpu.idle) * 100
            )
        |> avg
        |> round
