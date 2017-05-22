-module(cmaggregate_server).
-behaviour(gen_server).
-export([start_link/2]).
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).
-record(state, {mod, sup, topic}).

start_link(Mod, Sup) ->
  gen_server:start_link(?MODULE, [Mod, Sup], []).

init([Mod, Sup]) ->
  TopicSpec = Mod:topic(),
  case TopicSpec of
    {any_worker, _} ->
      {ok, #state{mod=Mod, sup=Sup, topic=TopicSpec}, 2000};
    {_, T} ->
      cmcluster:sub(T),
      {ok, #state{mod=Mod, sup=Sup, topic=TopicSpec}}
  end.

handle_info(timeout, #state{mod=Mod, topic=TopicSpec}=State) ->
  {any_worker, T} = TopicSpec,
  Workers = erlang:system_info(schedulers_online),
  Sup = cmaggregate_worker_sup:registered_name(Mod),
  [supervisor:start_child(Sup, [T])|| _ <- lists:seq(1, Workers)],
  {noreply, State};

handle_info(Msg, #state{mod=Mod, topic=TopicSpec}=State) ->
  case TopicSpec of 
    {server, _} ->
      Mod:handle(Msg);
    {one_worker, _} ->
      WorkerTopic = Mod:worker_topic(Msg),
      case cmcluster:subscribers(WorkerTopic) of
        [] -> 
          Sup = cmaggregate_worker_sup:registered_name(Mod),
          supervisor:start_child(Sup, [WorkerTopic]);
        _ ->
          cmcluster:pub(WorkerTopic,Msg)
      end
  end,
  {noreply, State}.

handle_call(_, _, State) ->
  {reply, ok, State}.

handle_cast(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
