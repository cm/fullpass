-module(cmstore_db).
-behaviour(gen_statem).
-export([start_link/0]).
-export([init/1, callback_mode/0, terminate/3]).
-export([ready/3]).
-record(data, {db, env}).

callback_mode() ->
    state_functions.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Env} = elmdb:env_open("/tmp/lmdb1", []),
    {ok, Db} = elmdb:db_open(Env, [create]),
    cmkit:log({cmstore_db, node(), started}),
    {ok, ready, #data{db=Db, env=Env}}.

ready({call, From}, {put, K, V}, #data{db=Db}=Data) ->
    Res = elmdb:put(Db, K, erlang:term_to_binary(V)),
    {keep_state, Data, {reply, From, Res}};

ready({call, From}, {get, K}, #data{db=Db}=Data) ->
    Res = case elmdb:get(Db, K) of 
              not_found -> not_found;
              {ok, V} -> {ok, erlang:binary_to_term(V)}
          end,
    {keep_state, Data, {reply, From, Res}}.

terminate(Reason, _, #data{}) ->
    cmkit:log({cmstore_db, node(), terminated, Reason}),
    ok.
