-module(cmaggregate_server).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).
-record(state, {mod, topic, extra}).

start_link(Mod) ->
  gen_server:start_link(?MODULE, [Mod], []).

init([Mod]) ->
  TopicSpec = Mod:topic(),
  case TopicSpec of
    {server, T} ->
      ExtraState = Mod:init(),
      cmcluster:sub(T),
      {ok, #state{mod=Mod, topic=TopicSpec, extra=ExtraState}};
    {any_worker, _} ->
      {ok, #state{mod=Mod, topic=TopicSpec}};
    {one_worker, T} ->
      cmcluster:sub(T),
      {ok, #state{mod=Mod, topic=TopicSpec}}
  end.

handle_info(Msg, #state{mod=Mod, topic=TopicSpec, extra=ExtraState}=State) ->
  case TopicSpec of 
    {server, _} ->
      NewState = Mod:handle(Msg, ExtraState),
      {noreply, #state{extra=NewState}};
    {one_worker, _} ->
      WorkerTopic = Mod:worker_topic(Msg),
      case cmcluster:subscribers(WorkerTopic) of
        [] -> 
          Sup = cmaggregate_worker_sup:registered_name(Mod),
          supervisor:start_child(Sup, [Msg]);
        _ ->
          cmcluster:pub(WorkerTopic,Msg)
      end,
      {noreply, State}
  end.

handle_call(_, _, State) ->
  {reply, ok, State}.

handle_cast(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
