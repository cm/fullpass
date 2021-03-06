module Init exposing (..)

import Commands exposing (..)
import Date
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
    , node = Nothing
    , nodeTable = Nothing
    , table = Nothing
    , hostname = Nothing
    , media = Nothing
    , newTableData = Nothing
    , nodeSelection = Nothing
    , tableMediaSelection = Nothing
    , events = []
    , backups = []
    , newBackupData = Nothing
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


selectedNode : Model -> Maybe NodeView
selectedNode model =
    case model.node of
        Nothing ->
            Nothing

        Just n ->
            Dict.get n model.nodes


selectedNodeTable : Model -> NodeView -> Maybe TableData
selectedNodeTable model v =
    case model.nodeTable of
        Nothing ->
            Nothing

        Just n ->
            nodeTable n v.node


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
                                    case Dict.get t.info.name j of
                                        Nothing ->
                                            tableView t n

                                        Just v ->
                                            tableViewWithNode v t n
                            in
                            Dict.insert t.info.name view j
                    )
                    i
                    n.db.tables
        )
        model.tables
        nodes


indexedTableDefinitions : Model -> List TableInfo -> Dict String TableView
indexedTableDefinitions model tables =
    List.foldl
        (\t ->
            \i ->
                let
                    v =
                        tableViewFromInfo t
                in
                Dict.insert t.name v i
        )
        Dict.empty
        tables


tableNames : Model -> List String
tableNames model =
    model.tables
        |> Dict.values
        |> List.sortWith tableByName
        |> List.map
            (\v ->
                v.table.info.name
            )


nodeTable : String -> NodeData -> Maybe TableData
nodeTable name node =
    node.db.tables
        |> List.filter
            (\t ->
                t.info.name == name
            )
        |> List.head


tableGroup : TableData -> NodeData -> Maybe NodeGroup
tableGroup table node =
    case nodeTable table.info.name node of
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


tableViewFromInfo : TableInfo -> TableView
tableViewFromInfo d =
    { table =
        { info = d
        , size =
            { count = 0
            , words = 0
            }
        , copies =
            { disc = []
            , mem = []
            , both = []
            }
        }
    , groups = Dict.empty
    }


groups : TableView -> List NodeGroup
groups v =
    v.groups
        |> Dict.values


compareNodeByName : NodeView -> NodeView -> Order
compareNodeByName n1 n2 =
    compare n1.node.info.hostname n2.node.info.hostname


tableByName : TableView -> TableView -> Order
tableByName n1 n2 =
    compare n1.table.info.name n2.table.info.name


nodes : Model -> List NodeView
nodes model =
    model.nodes
        |> Dict.values
        |> List.sortWith compareNodeByName


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


newDummyTableReplica : Dict String TableReplicaData
newDummyTableReplica =
    Dict.insert "node1" { node = "node1" } Dict.empty


newTableData : NewTableData
newTableData =
    { name = ""
    , storage = defaultTableStorage
    , media = defaultTableMedia
    , replicas = Dict.empty
    }


modelWithNewTable : Model -> NewTableData -> String -> TableStorage -> TableMedia -> Model
modelWithNewTable model d name storage media =
    let
        d2 =
            { d
                | name = name
                , storage = storage
                , media = media
            }
    in
    { model | newTableData = Just d2 }


modelWithNewTableStorage : Model -> NewTableData -> TableStorage -> Model
modelWithNewTableStorage model d v =
    let
        d2 =
            { d | storage = v }
    in
    { model | newTableData = Just d2 }


tableStorageToString : TableStorage -> String
tableStorageToString s =
    case s of
        Set ->
            "Set"

        Bag ->
            "Bag"

        OrderedSet ->
            "Ordered set"


toTableStorage : String -> Maybe TableStorage
toTableStorage s =
    case s |> String.toLower of
        "set" ->
            Just Set

        "bag" ->
            Just Bag

        "ordered set" ->
            Just OrderedSet

        _ ->
            Nothing


tableMedia : List TableMedia
tableMedia =
    [ Both, Memory, Disc ]


defaultTableMedia : TableMedia
defaultTableMedia =
    Both


tableStorage : List TableStorage
tableStorage =
    [ Set, Bag, OrderedSet ]


defaultTableStorage : TableStorage
defaultTableStorage =
    Set


stringToTableMedia : String -> Maybe TableMedia
stringToTableMedia str =
    case str of
        "Disc only" ->
            Just Disc

        "Memory only" ->
            Just Memory

        "Memory and disc" ->
            Just Both

        "both" ->
            Just Both

        "memory" ->
            Just Memory

        "disc" ->
            Just Disc

        _ ->
            Nothing


tableMediaToString : TableMedia -> String
tableMediaToString m =
    case m of
        Disc ->
            "Disc only"

        Memory ->
            "Memory only"

        Both ->
            "Memory and disc"


tableMediaToId : TableMedia -> String
tableMediaToId m =
    case m of
        Disc ->
            "disc"

        Memory ->
            "memory"

        Both ->
            "both"


newTableReplicas : NewTableData -> List TableReplicaData
newTableReplicas d =
    d.replicas
        |> Dict.values
        |> List.sortWith
            (\r1 ->
                \r2 ->
                    compare r1.node r2.node
            )


modelWithoutNewTableReplica : Model -> NewTableData -> TableReplicaData -> Model
modelWithoutNewTableReplica model t r =
    let
        r2 =
            Dict.remove r.node t.replicas

        t2 =
            { t | replicas = r2 }
    in
    { model | newTableData = Just t2 }


nodeSelection : Model -> Maybe NodeView
nodeSelection model =
    case model.nodeSelection of
        Nothing ->
            model |> nodes |> List.head

        Just n ->
            Dict.get n model.nodes


modelWithNewTableReplica : Model -> NewTableData -> NodeView -> Model
modelWithNewTableReplica model t v =
    let
        r =
            t.replicas

        hostname =
            v.node.info.hostname

        r2 =
            Dict.insert hostname
                { node = hostname
                }
                r

        t2 =
            { t | replicas = r2 }
    in
    { model
        | newTableData = Just t2
        , nodeSelection = Nothing
        , tableMediaSelection = Nothing
    }


quorumSize : Model -> Int
quorumSize model =
    (model |> nodes |> List.length) // 2 + 1


hasEmptySchema : NodeData -> Bool
hasEmptySchema node =
    case node.db.tables of
        [ s ] ->
            case ( s.info.name, s.size.count, s.copies.disc, s.copies.both, s.copies.mem ) of
                ( "schema", 1, [], [], [ host ] ) ->
                    True

                _ ->
                    False

        _ ->
            False


infoMsg : String -> UserMsg
infoMsg text =
    { contents = text
    , severity = SevInfo
    }


canCreateTable : Model -> Bool
canCreateTable model =
    case model.newTableData of
        Nothing ->
            False

        Just t ->
            case t.name of
                "" ->
                    False

                _ ->
                    t.replicas |> Dict.isEmpty |> not


nodeByName : Model -> String -> Maybe NodeView
nodeByName model name =
    model.nodes |> Dict.get name


tableNodes : TableData -> List String
tableNodes t =
    t.copies.disc
        ++ t.copies.mem
        ++ t.copies.both


wordsToBytes : Int -> Int
wordsToBytes w =
    8 * w


events : Model -> List EventData
events model =
    model.events
        |> List.sortBy .date
        |> List.reverse


eventColor : EventData -> String
eventColor ev =
    case ev.kind of
        "mnesia_up" ->
            "success"

        "mnesia_down" ->
            "warning"

        _ ->
            "gray"


backups : Model -> List BackupData
backups model =
    model.backups
        |> List.sortBy .date
        |> List.reverse


backupColor : BackupData -> String
backupColor ev =
    "gray"


newBackupData : NewBackupData
newBackupData =
    { name = "" }


newBackupDataWithName : Maybe NewBackupData -> String -> NewBackupData
newBackupDataWithName data n =
    case data of
        Nothing ->
            let
                d =
                    newBackupData
            in
            { d | name = n }

        Just d ->
            { d | name = n }


canCreateBackup : Model -> Bool
canCreateBackup model =
    case model.newBackupData of
        Nothing ->
            False

        Just t ->
            case t.name |> String.trim of
                "" ->
                    False

                _ ->
                    True


humanDay : Date.Date -> String
humanDay d =
    [ Date.day d |> toString
    , Date.month d |> toString
    , Date.year d |> toString
    ]
        |> String.join " "


humanTime : Date.Date -> String
humanTime d =
    [ Date.hour d
    , Date.minute d
    , Date.second d
    ]
        |> List.map toString
        |> List.map
            (\s ->
                String.padLeft 2 '0' s
            )
        |> String.join ":"


humanDate : Int -> String
humanDate millis =
    let
        d =
            millis
                |> toFloat
                |> Date.fromTime
    in
    [ humanDay d
    , humanTime d
    ]
        |> String.join " "
