-module(cmdb_client).
-behaviour(gen_statem).
-export([
         start_link/1,
         init/1, 
         callback_mode/0, 
         terminate/3,
         ready/3
        ]).
-record(data, {endpoints, db}).
-define(JSON, "application/json").
callback_mode() ->
    state_functions.

start_link(#{ name := Name }=Db) ->
    gen_statem:start_link({local, Name}, ?MODULE, [Db], []).

init([#{name := Name }=Db]) ->
    Urls = urls(Db),
    cmkit:log({cmdb, Name, http, started}),
    {ok, ready, #data{endpoints=Urls, db=Db}}.

ready({call, From}, {get, K}, #data{endpoints=[E1|_]}=Data) ->
    Url = <<E1/binary, <<"/">>/binary, K/binary>>,
    Res = cmhttp:get(Url),
    {keep_state, Data, {reply, From, Res}};

ready({call, From}, {put, K, V}, #data{endpoints=[E1|_]}=Data) ->
    Url = <<E1/binary, <<"/">>/binary, K/binary>>,
    Res = cmhttp:post(Url, ?JSON, V),
    {keep_state, Data, {reply, From, Res}};

ready({call, From}, {put, _Pairs}, #data{endpoints=[_E1|_]}=Data) ->
    Res = {error, not_implemented},
    {keep_state, Data, {reply, From, Res}}.

terminate(Reason, _, #data{}) ->
    cmkit:log({cmdb_db, node(), terminated, Reason}),
    ok.

urls(#{ scheme := Scheme,
       hosts := Hosts,
       port := Port,
       path := Path }) ->
    [ cmhttp:url(Scheme, Host, Port, Path) || Host <- Hosts].
