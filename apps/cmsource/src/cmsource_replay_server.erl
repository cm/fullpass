-module(cmsource_replay_server).
-behaviour(cmsource).
-export([start_link/0, handle/1, handle/2, mode/0]).

start_link() ->
  cmsource:start_link(?MODULE, event, replay, replay).

mode() -> read.

handle(_Msg) ->
  ok.

handle(_, Data) ->
  [cmcluster:dispatch(T, Msg) || {T, _}=Msg <- Data],
  ok.


