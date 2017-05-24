-module(cmaggregate).
-behaviour(supervisor).
-export([start_link/1, init/1]).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [{mode, 0}, {topic, 0}, {topic, 1}, {init, 1}, {handle, 2}].

start_link(Module) ->
  supervisor:start_link(?MODULE, [Module]).

init([Module]) ->
  Mode = Module:mode(),
  case Mode of
    any_worker ->
      {ok, {{one_for_one, 1, 5}, [
                                  cmkit:child_spec(w1, cmaggregate_worker, [Module], worker),
                                  cmkit:child_spec(w2, cmaggregate_worker, [Module], worker),
                                  cmkit:child_spec(w3, cmaggregate_worker, [Module], worker),
                                  cmkit:child_spec(w4, cmaggregate_worker, [Module], worker)
                                 ]}};
    server ->
      {ok, {{one_for_one, 1, 5}, [
                                  cmkit:child_spec(cmaggregate_server, [Module, Mode], worker)
                                 ]}};

    one_worker ->
      {ok, {{one_for_one, 1, 5}, [
                                  cmkit:child_spec(cmaggregate_server, [Module, Mode], worker),

                                  cmkit:child_spec(cmaggregate_worker_sup, [Module],supervisor)
                                 ]}}
  end.
