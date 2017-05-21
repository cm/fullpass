-module(cmsource_event_server).
-behaviour(cmsource).
-export([start_link/0, handle/1]).

start_link() ->
  cmsource:start_link(?MODULE, event).

handle({T, _, _}=Msg) ->
  cmcluster:dispatch(T, Msg).

