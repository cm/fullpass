-module(cmsource_query_server).
-behaviour(cmsource).
-export([start_link/0, handle/1]).

start_link() ->
  cmsource:start_link(?MODULE, query).

handle({T, _, _}=Msg) ->
  cmcluster:dispatch(T, Msg).
