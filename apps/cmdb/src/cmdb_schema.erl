-module(cmdb_schema).
-behaviour(gen_statem).
-export([start_link/0]).
-export([init/1, callback_mode/0, terminate/3]).
-export([red/3, yellow/3, green/3]).
-record(data, {db, env}).
-define(APP, "schema").
-define(NS, "cmdb").

callback_mode() ->
    state_functions.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    net_kernel:monitor_nodes(true),
    net_adm:world(),
    {ok, Env, Db} = cmdb_util:open(?APP, ?NS),
    case length(cmdb_util:expected_nodes()) of
        1 ->
            cmkit:log({cmdb, green, standalone}),
            {ok, green, #data{db=Db, env=Env}};
        _ ->
            {ok, red, #data{}}
    end.

red(info, {nodedown, Node}, Data) ->
    State = cmdb_util:state(),
    cmkit:log({cmdb, State, nodedown, Node}),
    {next_state, State, Data};

red(info, {nodeup, Node}, Data) ->
    State = cmdb_util:state(),
    cmkit:log({cmdb, State, nodeup, Node}),
    {next_state, State, Data};

red(info, Msg, Data) ->
    State = cmdb_util:state(),
    cmkit:log({cmdb, State, ignored_message, Msg}),
    {next_state, State, Data}.

yellow(info, {nodedown, Node}, Data) ->
    State = cmdb_util:state(),
    cmkit:log({cmdb, State, nodedown, Node}),
    {next_state, State, Data};

yellow(info, {nodeup, Node}, Data) ->
    State = cmdb_util:state(),
    cmkit:log({cmdb, State, nodeup, Node}),
    {next_state, State, Data};

yellow(info, Msg, Data) ->
    State = cmdb_util:state(),
    cmkit:log({cmdb, State, ignored_message, Msg}),
    {next_state, State, Data}.

green(info, {nodedown, Node}, Data) ->
    State = cmdb_util:state(),
    cmkit:log({cmdb, State, nodedown, Node}),
    {next_state, State, Data};

green(info, {nodeup, Node}, Data) ->
    State = cmdb_util:state(),
    cmkit:log({cmdb, State, nodeup, Node}),
    {next_state, State, Data};

green(info, Msg, Data) ->
    State = cmdb_util:state(),
    cmkit:log({cmdb, State, ignored_message, Msg}),
    {next_state, State, Data};

green({call, From}, {add_db, App, Ns, Nodes}, #data{db=_Db}=Data) ->
    cmkit:log({cmdb_schema, add_db, App, Ns, Nodes}),
    Res = ok,
    {keep_state, Data, {reply, From, Res}};

green({call, From}, Msg, #data{db=_Db}=Data) ->
    cmkit:log({cmdb_schema, green, ignored_message, Msg}),
    Res = ignored,
    {keep_state, Data, {reply, From, Res}}.

terminate(Reason, _, #data{env=Env}) ->
    cmkit:log({cmdb, closing, ?APP, ?NS, Reason}),
    cmdb_util:close(?APP, ?NS, Env). 
