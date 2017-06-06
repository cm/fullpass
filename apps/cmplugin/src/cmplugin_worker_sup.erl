-module(cmplugin_worker_sup).
-behaviour(supervisor).
-export([start_link/1, registered_name/1]).
-export([init/1]).

registered_name(Mod) ->
  list_to_atom(atom_to_list(Mod) ++ "worker_sup").

start_link(Mod) ->
  supervisor:start_link({local, registered_name(Mod)}, ?MODULE, [Mod]).

init([Mod]) ->
  {ok, {{simple_one_for_one, 1, 5}, [
                                     cmkit:child_spec(cmplugin_worker, [Mod], worker)
                                    ]}}.
