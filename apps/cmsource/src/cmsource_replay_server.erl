-module(cmsource_replay_server).
-behaviour(cmsource).
-export([start_link/0, handle/1, handle/2, mode/0]).

start_link() ->
  cmsource:start_link(?MODULE, event, replay).

mode() -> read.

handle(_Msg) ->
  ok.

handle(Msg, Data) ->
  io:format("handling ~p~nwith data ~p~n", [Msg, Data]),
  ok.


