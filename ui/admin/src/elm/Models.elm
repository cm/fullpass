module Models exposing (..)

import Dict exposing (..)
import FileReader exposing (..)


type alias Flags =
    { api : String
    , ws : String
    , files : String
    }


type Perspective
    = Servers
    | Database


type State
    = SignedOut
    | SignedIn


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


type NodeTab
    = Connections
    | Tables


type alias NodeView =
    { node : NodeData
    , tab : NodeTab
    }


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
    , perspective : Perspective
    , node : Maybe NodeView
    , table : Maybe TableView
    }
