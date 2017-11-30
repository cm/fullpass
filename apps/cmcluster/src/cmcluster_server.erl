-module(cmcluster_server).
-behaviour(gen_statem).
-export([start_link/0]).
-export([init/1, callback_mode/0, terminate/3]).
-export([all_nodes/0, join/1]).
-export([ready/3]).
-record(data, {name, cluster, db}).

callback_mode() ->
    state_functions.

start_link() ->
    NodeName = cmkit:config(nodename, cmcluster),
    ClusterName = cmkit:config(cluster, cmcluster),
    WorkDir = cmkit:config(workdir, cmcluster),
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [NodeName, 
                                                      ClusterName, 
                                                      WorkDir], []).

all_nodes() -> 
    {ok, gen_statem:call(?MODULE, nodes)}.

join(Node) -> 
    gen_statem:call(?MODULE, {join, Node}).

init([NodeName, ClusterName, WorkDir]) ->
    DbFile = filename:join([WorkDir, "data.db"]),
    case dets:open_file(NodeName, [{file, DbFile}]) of
        {ok, NodeName} ->
            cmkit:log({cmcluster, NodeName, ok}),
            {ok, ready, #data{name=NodeName, cluster=ClusterName, db=DbFile}};        
        {error, Reason} ->
            cmkit:log({cmcluster, NodeName, error, Reason}),
            {stop, Reason}             
    end.

ready({call, From}, nodes, Data) ->
    Nodes = info(nodes, Data),
    {keep_state,Data,[{reply,From, Nodes}]}.

terminate(Reason, _, #data{name=NodeName}) ->
    case dets:close(NodeName) of
        ok -> 
            cmkit:log({cmcluster, NodeName, closed, Reason});
        {error, Reason} ->
            cmkit:log({cmcluster, NodeName, error, Reason})
    end.

info(nodes, #data{cluster=ClusterName}) -> 
    {ok, Hostname} = inet:gethostname(),
    {ok, {hostent, Hostname, [], inet, 4, Ips}} = inet:gethostbyname(Hostname),
    [#{ name => cmkit:to_bin(node()),
        sname => cmkit:sname(),
        cluster => cmkit:to_bin(ClusterName),
        hostname => cmkit:to_bin(Hostname),
        ips => lists:map(fun cmkit:to_bin/1, Ips),
        perf => cmperf:stats() }].



