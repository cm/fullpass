-module(cmdb_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  case cmdb_sup:start_link() of
    {ok, Pid} ->
      riak_core:register([{vnode_module, cmdb_vnode}]),
      {ok, Ring} = riak_core_ring_manager:get_my_ring(),
      riak_core_ring_handler:ensure_vnodes_started(Ring),
      {ok, Pid};
    {error, E} ->
      {error, E}
  end.

stop(_State) ->
  ok.
