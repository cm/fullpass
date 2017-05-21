-module(cmsource_cmd_server).
-behaviour(cmsource).
-export([start_link/0, handle/1]).

start_link() ->
  cmsource:start_link(?MODULE, cmd).

handle({_, _, From}=Msg) ->
  From ! {ok, processing},
  cmcluster:event(Msg).
