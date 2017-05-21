-module(cmaggregate).
-behaviour(supervisor).
-export([start_link/1, init/1]).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [{topic, 0}, {init, 0}, {handle, 1}].

start_link(Mod) ->
  supervisor:start_link(?MODULE, [Mod]).

init([Mod]) ->
  {ok, {{one_for_one, 1, 5}, [
                              cmkit:child_spec(cmaggregate_server, [Mod, self()], worker),
                              
                              cmkit:child_spec(cmaggregate_worker_sup, [Mod, self()],supervisor)
                             ]}}.

