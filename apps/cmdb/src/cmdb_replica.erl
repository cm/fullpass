-module(cmdb_replica).
-behaviour(gen_statem).
-export([start_link/1]).
-export([init/1, callback_mode/0, terminate/3]).
-export([ready/3]).
-record(data, {db, env}).

callback_mode() ->
    state_functions.

start_link(#{ name := DbName}=Db) ->
    gen_statem:start_link({local, DbName}, ?MODULE, [Db], []).

init([#{ name := Name,
         size := Size,
         type := Type }]) ->
    {ok, Env, Db} = cmdb_util:open(Name, Type, Size),
    cmkit:log({cmdb_replica, Name, Type, node(), started}),
    {ok, ready, #data{db=Db, env=Env}}.

ready({call, From}, {put, K, V}, #data{db=Db}=Data) ->
    Res = cmdb_util:write(Db, K, V),
    {keep_state, Data, {reply, From, Res}};

ready({call, From}, {put, Pairs}, #data{env=Env, db=Db}=Data) ->
    Res = cmdb_util:write_all(Env, Db, Pairs),
    {keep_state, Data, {reply, From, Res}};

ready({call, From}, {get, K}, #data{db=Db}=Data) ->
    Res = cmdb_util:read(Db, K),
    {keep_state, Data, {reply, From, Res}}.

terminate(Reason, _, #data{}) ->
    cmkit:log({cmdb_db, node(), terminated, Reason}),
    ok.
