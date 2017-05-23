-module(cmaggregate).
-behaviour(supervisor).
-export([start_link/1, init/1]).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [{topic, 0}, {init, 0}, {handle, 2}].

start_link(Mod) ->
  supervisor:start_link(?MODULE, [Mod]).

init([Mod]) ->
  TopicSpec = Mod:topic(),
  case TopicSpec of
    {any_worker, _} ->
      {ok, {{one_for_one, 1, 5}, [
        cmkit:child_spec(w1, cmaggregate_worker, [Mod], worker),
        cmkit:child_spec(w2, cmaggregate_worker, [Mod], worker),
        cmkit:child_spec(w3, cmaggregate_worker, [Mod], worker),
        cmkit:child_spec(w4, cmaggregate_worker, [Mod], worker)
                                 ]}};
    {server, _} ->
      {ok, {{one_for_one, 1, 5}, [
        cmkit:child_spec(cmaggregate_server, [Mod], worker)
      ]}};

    {one_worker, _} ->
      {ok, {{one_for_one, 1, 5}, [
                              cmkit:child_spec(cmaggregate_server, [Mod], worker),
                              
                              cmkit:child_spec(cmaggregate_worker_sup, [Mod],supervisor)
                             ]}}
  end.



