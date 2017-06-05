-module(cmdb_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  Children = [
              {cmdb_vnode_master,
               {riak_core_vnode_master, start_link, [cmdb_vnode]},
               permanent, 5000, worker, [riak_core_vnode_master]},
              
              {cmdb_fsm_sup,
               {cmdb_fsm_sup, start_link, []},
               permanent, infinity, supervisor, [cmdb_fsm_sup]}
             ],
  {ok, {{one_for_one, 0, 1}, Children}}.
