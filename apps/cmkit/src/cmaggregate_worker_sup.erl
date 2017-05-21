-module(cmaggregate_worker_sup).
-behaviour(supervisor).
-export([start_link/2, registered_name/1]).
-export([init/1]).

registered_name(Mod) ->
  list_to_atom(atom_to_list(Mod) ++ "worker_sup").

start_link(Mod, Parent) ->
  supervisor:start_link({local, registered_name(Mod)}, ?MODULE, [Mod, Parent]).

init([Mod, _]) ->
  {ok, {{simple_one_for_one, 1, 5}, [
                                     cmkit:child_spec(cmaggregate_worker, [Mod], worker)
                                    ]}}.
