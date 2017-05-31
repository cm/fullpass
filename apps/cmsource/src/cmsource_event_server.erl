-module(cmsource_event_server).
-behaviour(cmsource).
-export([start_link/0, handle/1, handle/2, mode/0]).

start_link() ->
  cmsource:start_link(?MODULE, event).

mode() -> write.

handle(_, _) -> ok.

handle({T, _, _}=Msg) ->
  cmcluster:dispatch(T, Msg).

