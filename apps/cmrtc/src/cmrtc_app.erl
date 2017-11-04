-module(cmrtc_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %[cmdb:c(T) || T <- tables()],
    cmrtc_sup:start_link().

stop(_State) ->
  ok.

