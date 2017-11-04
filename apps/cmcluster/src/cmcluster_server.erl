-module(cmcluster_server).
-behaviour(gen_statem).
-export([start_link/0]).
-export([init/1, callback_mode/0, terminate/3]).
-export([all_nodes/0, join/1]).
-export([ready/3]).
-record(data, {name, db}).

callback_mode() ->
    state_functions.

start_link() ->
    NodeName = cmkit:config(nodename, cmcluster),
    WorkDir = cmkit:config(workdir, cmcluster),
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [NodeName, WorkDir], []).

all_nodes() -> 
    {ok, gen_statem:call(?MODULE, nodes)}.

join(Node) -> 
    gen_statem:call(?MODULE, {join, Node}).

init([NodeName, WorkDir]) ->
    DbFile = filename:join([WorkDir, "data.db"]),
    case dets:open_file(NodeName, [{file, DbFile}]) of
        {ok, NodeName} ->
            cmkit:log({cmcluster, NodeName, ok}),
            {ok, ready, #data{name=NodeName, db=DbFile}};        
        {error, Reason} ->
            cmkit:log({cmcluster, NodeName, error, Reason}),
            {stop, Reason}             
    end.

ready({call, From}, nodes, Data) ->
    Nodes = [node()|nodes()],
    {keep_state,Data,[{reply,From, Nodes}]}.

terminate(Reason, _, #data{name=NodeName}) ->
    case dets:close(NodeName) of
        ok -> 
            cmkit:log({cmcluster, NodeName, closed, Reason});
        {error, Reason} ->
            cmkit:log({cmcluster, NodeName, error, Reason})
    end.
