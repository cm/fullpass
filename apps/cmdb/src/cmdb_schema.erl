-module(cmdb_schema).
-behaviour(gen_statem).
-export([start_link/0]).
-export([init/1, callback_mode/0, terminate/3]).
-export([red/3, yellow/3, green/3]).
-record(data, {}).

callback_mode() ->
    state_functions.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    net_kernel:monitor_nodes(true),
    net_adm:world(),
    case length(cmdb_util:expected_nodes()) of
        1 ->
            cmkit:log({cmdb, started, green, standalone}),
            {ok, green, #data{}};
        _ ->
            cmkit:log({cmdb, started, red, cmdb_util:expected_nodes()}),
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

green({call, From}, Msg, Data) ->
    cmkit:log({cmdb_schema, green, ignored_message, Msg}),
    Res = ignored,
    {keep_state, Data, {reply, From, Res}}.

terminate(Reason, _, _) ->
    cmkit:log({cmdb_schema, terminate, Reason}),
    ok.
