-module(cmdb_cloud).
-behaviour(gen_statem).
-export([
         start_link/0,
         init/1, 
         callback_mode/0, 
         terminate/3,
         ready/3
        ]).
-record(data, {}).

callback_mode() ->
    state_functions.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    net_kernel:monitor_nodes(true),
    net_adm:world(),
    cmkit:log({cmdb, cmcloud:state(), node()}),
    {ok, ready, #data{}}.

ready(info, {nodedown, Node}, Data) ->
    State = cmcloud:state(),
    cmkit:log({cmdb, State, nodedown, Node}),
    {next_state, ready, Data};

ready(info, {nodeup, Node}, Data) ->
    State = cmcloud:state(),
    cmkit:log({cmdb, State, nodeup, Node}),
    {next_state, ready, Data};

ready(info, Msg, Data) ->
    State = cmcloud:state(),
    cmkit:log({cmdb, State, message, Msg}),
    {next_state, ready, Data}.

terminate(Reason, _, _) ->
    cmkit:log({cmdb, terminate, Reason}),
    ok.
