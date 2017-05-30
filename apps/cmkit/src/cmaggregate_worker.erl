-module(cmaggregate_worker).
-behaviour(gen_server).
-export([start_link/1, start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {mod, topic, data}).

start_link(Module) ->
  Topic =  Module:topic(),
  gen_server:start_link(?MODULE, [Module, Topic], []).

start_link(Module, Topic, Msg) ->
  gen_server:start_link(?MODULE, [Module, Topic, Msg], []).

init([Module, T]) ->
  D = Module:init(T),
  cmcluster:sub(T),
  {ok, #state{mod=Module, topic=T, data=D}};

init([Module, T, Msg]) ->
  D = Module:init(T),
  D2 = Module:handle(Msg, D),
  cmcluster:sub(T),
  {ok, #state{mod=Module, topic=T, data=D2}}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(Msg, #state{mod=Module, data=D}=State) ->
  D2=Module:handle(Msg, D),
  {noreply, State#state{data=D2}}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

