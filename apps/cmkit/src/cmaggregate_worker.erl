-module(cmaggregate_worker).
-behaviour(gen_server).
-export([start_link/1, start_link/3, start_link/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {mod, topic, data}).

start_link(Module) ->
  Topic =  Module:topic(),
  gen_server:start_link(?MODULE, [Module, Topic], []).

start_link(Module, Topic, Msg) ->
  gen_server:start_link(?MODULE, [Module, Topic, Msg], []).

start_link(Module, Topic, Msg, Timeout) ->
  gen_server:start_link(?MODULE, [Module, Topic, Msg, Timeout], []).

init([Module, T]) ->
  D = Module:init(T),
  cmcluster:sub(T),
  {ok, #state{mod=Module, topic=T, data=D}};

init([Module, T, Msg]) ->
  D = Module:init(T),
  D2 = Module:handle(Msg, D),
  cmcluster:sub(T),
  {ok, #state{mod=Module, topic=T, data=D2}};

init([Module, T, Msg, Timeout]) ->
  {ok, State} = init([Module, T, Msg]),
  {ok, State, Timeout}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, #state{mod=Module, data=D}=State) ->
  D2=Module:timeout(D),
  {noreply, State#state{data=D2}};

handle_info(Msg, #state{mod=Module, topic=T, data=D}=State) ->
  case Module:handle(Msg, D) of
    {continue, D2} ->
      {noreply, State#state{data=D2}};
    {stop, D2} ->
      {stop, {timeout, T}, State#state{data=D2}}
  end. 

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
