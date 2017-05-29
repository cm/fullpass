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
       cmcluster:cmd({login, Code})
   end, 
   fun(_, _, {_, _}) -> 
       continue;
      (_, _, SessionId) -> 
       {redirect, 
        cmkit:config(app_url, ?APP), #{<<"cmtoken">> => SessionId}}
   end};

do([<<"session">>]) ->
  {anon, [{text, <<"id">>}],
   fun(_, _, [Id]) ->
       cmcluster:query({{session, Id}, none})
   end,
   fun(_, _, {ok, processing}) ->
      continue;
      (_, _, Profile) ->
       {data, Profile}
   end};

do(_) -> no_implemented.

