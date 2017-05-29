-module(cmaggregate_server).
-behaviour(gen_server).
-export([start_link/2]).
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).
-record(state, {module, mode, topic, data}).

start_link(Module, Mode) ->
  gen_server:start_link(?MODULE, [Module, Mode], []).

init([Module, Mode]) ->
  cmcluster:sub(no_subscribers),
  case Mode of
    server ->
      T = Module:topic(),
      Data = Module:init(T),
      cmcluster:sub(T),
      {ok, #state{module=Module, mode=Mode, topic=T, data=Data}};
    any_worker ->
      T = Module:topic(),
      {ok, #state{module=Module, mode=Mode, topic=T}};
    one_worker ->
      T = Module:topic(),
      cmcluster:sub(T),
      {ok, #state{module=Module, mode=Mode, topic=T}}
  end.

handle_info({no_subscribers, Msg}, #state{module=Module}=State) ->
  Module:missing(Msg),
  {noreply, State};

handle_info(Msg, #state{module=Module, mode=Mode, data=Data}=State) ->
  case Mode of 
    server ->
      NewData = Module:handle(Msg, Data),
      {noreply, #state{data=NewData}};
    one_worker ->
      WorkerT = Module:topic(Msg),
      case cmcluster:who(WorkerT) of
        [] -> 
          Sup = cmaggregate_worker_sup:registered_name(Module),
          supervisor:start_child(Sup, [WorkerT,Msg]);
        _ ->
          cmcluster:pub(WorkerT,Msg)
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
