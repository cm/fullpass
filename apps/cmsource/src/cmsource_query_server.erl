-module(cmsource_query_server).
-behaviour(cmsource).
-export([start_link/0, handle/1, handle/2, mode/0]).

start_link() ->
  cmsource:start_link(?MODULE, query).

mode() -> write.

handle({T, _, _}=Msg) ->
  cmcluster:dispatch(T, Msg).

handle(_, _) -> ok.
