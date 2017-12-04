-module(cmcluster_server).
-behaviour(gen_statem).
-export([start_link/0]).
-export([init/1, callback_mode/0, terminate/3]).
-export([expected_nodes/0, all_nodes/0]).
-export([red/3, yellow/3, green/3]).
-record(data, {}).

callback_mode() ->
    state_functions.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

all_nodes() -> 
    Members = pg2:get_members(cmcluster),
    {ok, lists:map(fun(M) ->
                           gen_statem:call(M, nodes)
                   end, Members)}.

init([]) ->
    pg2:create(cmcluster),
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
    {ok, JoinStatus} = join(State),
    cmkit:log({cmcluster, State, nodeup, Node, JoinStatus}),
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
    {ok, JoinStatus} = join(State),
    cmkit:log({cmcluster, State, nodeup, Node, JoinStatus}),
    {next_state, State, Data};

green(info, {nodedown, Node}, Data) ->
    State = state(),
    cmkit:log({cmcluster, State, nodedown, Node}),
    {next_state, State, Data};

green({call, From}, nodes, Data) ->
    Nodes = info(nodes, nothing),
    {keep_state, Data, {reply, From, Nodes}}.


join(green) ->
    LocalMembers = pg2:get_local_members(cmcluster),
    case lists:member(self(), LocalMembers) of
        true -> 
            {ok, already_in_cluster};
        false ->
            pg2:join(cmcluster, self()),
            {ok, joined_cluster}
    end;

join(_) -> 
    {ok, waiting_to_join_cluster}.

expected_nodes() ->
    Sname = erlang:binary_to_list(cmkit:sname()),
    [ erlang:list_to_atom(
        string:join([ Sname,
                      erlang:atom_to_list(H)
                    ], "@")
       ) || H <- net_adm:host_file()].


state() ->
    ExpectedNodes = expected_nodes(),
    Nodes = cmkit:intersection(ExpectedNodes, [node()|nodes()]),
    state(length(Nodes), length(ExpectedNodes)).

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
        health => state(),
        perf => cmperf:stats() }].
