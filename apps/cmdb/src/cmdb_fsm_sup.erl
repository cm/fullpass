-module(cmdb_fsm_sup).
-behaviour(supervisor).
-export([start/1, start_link/0]).
-export([init/1]).

start(Args) ->
  supervisor:start_child(?MODULE, Args).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  R = cmkit:config(reads, cmdb, 1),
  W = cmkit:config(writes, cmdb, 1),
  Fsm = {undefined,
              {cmdb_fsm, start_link, [R, W]},
              temporary, 5000, worker, [cmdb_fsm]},
  {ok, {{simple_one_for_one, 10, 10}, [Fsm]}}.
