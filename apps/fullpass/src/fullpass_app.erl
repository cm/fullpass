-module(fullpass_app).
-behaviour(application).
-export([start/2, stop/1, do/1]).
-define(APP, fullpass).

start(_StartType, _StartArgs) ->
  cmhttp:start(?APP, ?MODULE),
  fullpass_sup:start_link().

stop(_State) ->
  ok.

do([<<"ping">>]) -> 
  {data, #{<<"message">> => <<"pong">>}};

do([<<"info">>]) ->
  {data, cmperf:stats()};

do([<<"login">>]) ->
  {anon, [{text, <<"code">>}], 
   fun(_, _, [Code]) ->
       cmdb:cast(login, [Code, self()])
   end, 
   fun(_, _, SessionId) -> 
       {redirect, 
        cmkit:config(app_url, ?APP), 
        #{<<"cmtoken">> => SessionId}}
   end};

do([<<"session">>]) ->
  {anon, [{text, <<"id">>}],
   fun(_, _, [Id]) ->
       cmdb:cast({session, Id}, [Id, self()])
   end,
   fun(_, _, Profile) ->
       {data, Profile}
   end};

do(_) -> no_implemented.

