module Models exposing (..)

import Dict exposing (..)
import FileReader exposing (..)


type alias Flags =
    { api : String
    , ws : String
    , files : String
    }


type State
    = SignedOut
    | Tables
    | NewTable
    | Table
    | Nodes
    | Node
    | NodeTable
    | CreatingTable
    | CreatingSchema
    | DeletingSchema


type alias LoginData =
    { email : String
    , password : String
    }


type Severity
    = SevInfo
    | SevWarn
    | SevError


type alias UserMsg =
    { contents : String
    , severity : Severity
    }


type alias UserData =
    { first : String }


type alias NodeView =
    { node : NodeData
    , table : Maybe TableData
    , state : NodeState
    }


type NodeState
    = Idle
    | DeletingReplica
    | AddingReplica


type alias NodeData =
    { info : InfoData
    , cluster : ClusterData
    , perf : PerfData
    , db : DbData
    }


type alias TableView =
    { table : TableData
    , groups : Dict String NodeGroup
    }


type alias NodeGroup =
    { name : String
    , all : List String
    , mem : List String
    , disc : List String
    , both : List String
    }


type alias InfoData =
    { hostname : String
    , ips : List String
    }


type alias ClusterData =
    { health : String
    , peers : List String
    }


type alias DbData =
    { started : Bool
    , tables : List TableData
    }


type alias TableData =
    { id : String
    , name : String
    , kind : String
    , size : TableSizeData
    , copies : TableCopiesData
    }


type alias TableSizeData =
    { count : Int
    , words : Int
    }


type alias TableCopiesData =
    { disc : List String
    , mem : List String
    , both : List String
    }


type alias NewTableData =
    { name : String
    , storage : TableStorage
    , replicas : Dict String TableReplicaData
    }


type alias TableReplicaData =
    { node : String
    , media : TableMedia
    }


type TableStorage
    = Bag
    | Set
    | OrderedSet


type TableMedia
    = Disc
    | Memory
    | Both


type alias PerfData =
    { cpu : List CpuData
    , mem : MemData
    }


type alias CpuData =
    { num : Int
    , busy : Float
    , idle : Float
    }


type alias MemData =
    { free : Int
    , total : Int
    }


type alias Model =
    { flags : Flags
    , loginData : LoginData
    , userMsg : Maybe UserMsg
    , session : String
    , user : Maybe UserData
    , state : State
    , nodes : Dict String NodeView
    , tables : Dict String TableView
    , node : Maybe String
    , nodeTable : Maybe String
    , table : Maybe TableView
    , hostname : Maybe String
    , media : Maybe String
    , newTableData : Maybe NewTableData
    , nodeSelection : Maybe String
    , tableMediaSelection : Maybe TableMedia
    }
