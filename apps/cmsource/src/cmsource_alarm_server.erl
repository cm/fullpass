-module(cmsource_alarm_server).
-behaviour(cmsource).
-export([start_link/0, handle/1, handle/2, mode/0]).

start_link() ->
  cmsource:start_link(?MODULE, alarm).

mode() -> write.

handle(_) ->
  ok.

handle(_, _) ->
  ok.
