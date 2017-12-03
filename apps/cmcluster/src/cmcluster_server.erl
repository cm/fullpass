-module(cmcluster_server).
-behaviour(gen_statem).
-export([start_link/0]).
-export([init/1, callback_mode/0, terminate/3]).
-export([all_nodes/0]).
-export([red/3, yellow/3, green/3]).
-record(data, {}).

callback_mode() ->
    state_functions.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

all_nodes() -> 
    {ok, gen_statem:call(?MODULE, nodes)}.

init([]) ->
    net_kernel:monitor_nodes(true),
    net_adm:world(),
    {ok, red, #data{}}.

red(info, {nodeup, Node}, Data) ->
    State = state(),
    cmkit:log({cmcluster, State, nodeup, Node}),
    {next_state, State, Data};

red(info, {nodedown, Node}, Data) ->
    State = state(),
    cmkit:log({cmcluster, State, nodedown, Node}),
    {next_state, State, Data};

red({call, From}, nodes, Data) ->
    Nodes = info(nodes, nothing),
    {keep_state, Data, {reply, From, Nodes}}.


yellow(info, {nodeup, Node}, Data) ->
    State = state(),
    cmkit:log({cmcluster, State, nodeup, Node}),
    {next_state, State, Data};

yellow(info, {nodedown, Node}, Data) ->
    State = state(),
    cmkit:log({cmcluster, State, nodedown, Node}),
    {next_state, State, Data};

yellow({call, From}, nodes, Data) ->
    Nodes = info(nodes, nothing),
    {keep_state, Data, {reply, From, Nodes}}.

green(info, {nodeup, Node}, Data) ->
    State = state(),
    cmkit:log({cmcluster, State, nodeup, Node}),
    {next_state, State, Data};

green(info, {nodedown, Node}, Data) ->
    State = state(),
    cmkit:log({cmcluster, State, nodedown, Node}),
    {next_state, State, Data};

green({call, From}, nodes, Data) ->
    Nodes = info(nodes, nothing),
    {keep_state, Data, {reply, From, Nodes}}.

state() ->
    Hosts = net_adm:host_file(),
    Nodes = cmkit:intersection(Hosts, nodes()),
    state(length(Nodes), length(Hosts)).

state(Nodes, Hosts) when Nodes >= Hosts ->
    green;

state(Nodes, Hosts) when Nodes >= Hosts/2 ->
    yellow;

state(_, _) -> 
    red.


terminate(Reason, _, #data{}) ->
    cmkit:log({cluster_server, node(), terminated, Reason}),
    ok.

info(nodes, _) -> 
    {ok, Hostname} = inet:gethostname(),
    {ok, {hostent, Hostname, _, inet, 4, Ips}} = inet:gethostbyname(Hostname),
    [#{ name => cmkit:to_bin(node()),
        sname => cmkit:sname(),
        hostname => cmkit:to_bin(Hostname),
        ips => lists:map(fun cmkit:to_bin/1, Ips),
        state => state(),
        perf => cmperf:stats() }].
