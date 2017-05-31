-module(cmsource_cmd_server).
-behaviour(cmsource).
-export([start_link/0, handle/1, handle/2, mode/0]).

start_link() ->
  cmsource:start_link(?MODULE, cmd).

mode() -> write.

handle({_, _, From}=Msg) ->
  From ! {ok, processing},
  cmcluster:event(Msg).

handle(_, _) -> ok.
