-module(cmsource_alarm_server).
-behaviour(cmsource).
-export([start_link/0, handle/1]).

start_link() ->
  cmsource:start_link(?MODULE, alarm).

handle(_) ->
  ok.
