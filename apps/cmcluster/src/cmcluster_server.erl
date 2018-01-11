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
    {ok, [ gen_statem:call({cmcluster_server, N}, nodes) || N <- net_adm:world() ]}.

init([]) ->
    net_kernel:monitor_nodes(true),
    net_adm:world(),
    cmdb:subscribe(),
    case length(expected_nodes()) of
        1 -> 
            cmkit:log({cmcluster, green, standalone}),
            
            {ok, green, #data{}};
        _ -> 
            {ok, red, #data{}}
    end.

red(info, {nodeup, Node}, Data) ->
    State = state(),
    cmkit:log({cmcluster, State, nodeup, Node}),
    {next_state, State, Data};

red(info, {nodedown, Node}, Data) ->
    State = state(),
    cmkit:log({cmcluster, State, nodedown, Node}),
    {next_state, State, Data};

red(info, {mnesia_system_event, E}, Data) ->
    cmcluster:log(E),
    {next_state, red, Data};

red({call, From}, nodes, Data) ->
    Nodes = info(nodes, nothing),
    {keep_state, Data, {reply, From, Nodes}};

red({call, From}, stop_db, Data) ->
    Res= stop_db(),
    {keep_state, Data, {reply, From, Res}};

red({call, From}, start_db, Data) ->
    Res= start_db(),
    {keep_state, Data, {reply, From, Res}};

red({call, From}, drop_schema, Data) ->
    Res= drop_schema(),
    {keep_state, Data, {reply, From, Res}};

red({call, From}, create_schema, Data) ->
    Res= create_schema(),
    {keep_state, Data, {reply, From, Res}};

red({call, From}, {create_schema_replica, Node}, Data) ->
    Res = create_schema_replica(Node),
    {keep_state, Data, {reply, From, Res}};

red({call, From}, {create_table_replica, Node, Tab, Media}, Data) ->
    Res = create_table_replica(Node, Tab, Media),
    {keep_state, Data, {reply, From, Res}};

red({call, From}, {delete_table_replica, Node, Tab}, Data) ->
    Res = delete_table_replica(Node, Tab),
    {keep_state, Data, {reply, From, Res}}.


yellow(info, {nodeup, Node}, Data) ->
    State = state(),
    cmkit:log({cmcluster, State, nodeup, Node}),
    {next_state, State, Data};

yellow(info, {nodedown, Node}, Data) ->
    State = state(),
    cmkit:log({cmcluster, State, nodedown, Node}),
    {next_state, State, Data};

yellow(info, {mnesia_system_event, E}, Data) ->
    cluster:log(E),
    {next_state, yellow, Data};

yellow({call, From}, nodes, Data) ->
    Nodes = info(nodes, nothing),
    {keep_state, Data, {reply, From, Nodes}};

yellow({call, From}, stop_db, Data) ->
    Res= stop_db(),
    {keep_state, Data, {reply, From, Res}};

yellow({call, From}, start_db, Data) ->
    Res= start_db(),
    {keep_state, Data, {reply, From, Res}};

yellow({call, From}, drop_schema, Data) ->
    Res= drop_schema(),
    {keep_state, Data, {reply, From, Res}};

yellow({call, From}, create_schema, Data) ->
    Res= create_schema(),
    {keep_state, Data, {reply, From, Res}};

yellow({call, From}, {create_schema_replica, Node}, Data) ->
    Res = create_schema_replica(Node),
    {keep_state, Data, {reply, From, Res}};

yellow({call, From}, {create_table_replica, Node, Tab, Media}, Data) ->
    Res = create_table_replica(Node, Tab, Media),
    {keep_state, Data, {reply, From, Res}};

yellow({call, From}, {delete_table_replica, Node, Tab}, Data) ->
    Res = delete_table_replica(Node, Tab),
    {keep_state, Data, {reply, From, Res}}.

green(info, {nodeup, Node}, Data) ->
    State = state(),
    cmkit:log({cmcluster, State, nodeup, Node}),
    {next_state, State, Data};

green(info, {nodedown, Node}, Data) ->
    State = state(),
    cmkit:log({cmcluster, State, nodedown, Node}),
    {next_state, State, Data};

green(info, {mnesia_system_event, E}, Data) ->
    cmcluster:log(E),
    {next_state, green, Data};

green({call, From}, nodes, Data) ->
    Nodes = info(nodes, nothing),
    {keep_state, Data, {reply, From, Nodes}};

green({call, From}, stop_db, Data) ->
    Res= stop_db(),
    {keep_state, Data, {reply, From, Res}};

green({call, From}, start_db, Data) ->
    Res= start_db(),
    {keep_state, Data, {reply, From, Res}};

green({call, From}, drop_schema, Data) ->
    Res= drop_schema(),
    {keep_state, Data, {reply, From, Res}};

green({call, From}, create_schema, Data) ->
    Res= create_schema(),
    {keep_state, Data, {reply, From, Res}};

green({call, From}, {create_schema_replica, Node}, Data) ->
    Res = create_schema_replica(Node),
    {keep_state, Data, {reply, From, Res}};

green({call, From}, {create_table_replica, Node, Tab, Media}, Data) ->
    Res = create_table_replica(Node, Tab, Media),
    {keep_state, Data, {reply, From, Res}};

green({call, From}, {delete_table_replica, Node, Tab}, Data) ->
    Res = delete_table_replica(Node, Tab),
    {keep_state, Data, {reply, From, Res}}.

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
    #{  perf => cmperf:stats(), 
        info => #{ hostname => cmkit:to_bin(Hostname),
                   ips => cmkit:distinct(lists:map(fun cmkit:to_bin/1, Ips)) },
        cluster => #{ health => state(),
                      peers => lists:map(fun cmkit:node_host_short/1, nodes()) },
        db => #{ started => cmdb:started(),
                 tables => cmdb:tables_info()
               }
     }.

stop_db() -> 
    cmdb:stop().

start_db() -> 
    cmdb:start().

drop_schema() -> 
    cmdb:drop_schema(node()).

create_schema() -> 
    cmdb:create_schema(node()).

create_table_replica(Node, Tab, Type) ->
    MnesiaType = table_copies_for(Type),
    cmdb:add(Node, Tab, MnesiaType).

delete_table_replica(Node, Tab) -> 
    cmdb:drop(Tab, Node).

create_schema_replica(Node) ->
    cmdb:add(Node).


table_copies_for(disc) -> disc_only_copies;
table_copies_for(memory) -> ram_copies;
table_copies_for(both) -> disc_copies.
