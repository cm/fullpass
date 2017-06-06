-module(cmplugin).
-behaviour(supervisor).
-export([start_link/1, init/1]).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [{key, 1}, {init, 1}, {data, 2}, {missing, 1}].

start_link(Module) ->
  supervisor:start_link(?MODULE, [Module]).

init([Module]) ->
  {ok, {{one_for_one, 1, 5}, [
                              cmkit:child_spec(cmplugin_server, [Module], worker),
                              cmkit:child_spec(cmplugin_worker_sup, [Module],supervisor)
                             ]}}.
