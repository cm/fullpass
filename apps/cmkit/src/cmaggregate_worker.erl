-module(cmaggregate_worker).
-behaviour(gen_server).
-export([start_link/1, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {mod, topic, extra}).

start_link(Mod) ->
  Topic =  Mod:topic(),
  gen_server:start_link(?MODULE, [Mod, Topic], []).

start_link(Mod, Msg) ->
  Topic = Mod:worker_topic(Msg),
  gen_server:start_link(?MODULE, [Mod, Topic, Msg], []).

init([Mod, {_, Topic}]) ->
  ExtraState = Mod:init(),
  cmcluster:sub(Topic),
  {ok, #state{mod=Mod, topic=Topic, extra=ExtraState}};

init([Mod, {_, Topic}, Msg]) ->
  ExtraState = Mod:init(),
  NewState = Mod:handle(Msg, ExtraState),
  cmcluster:sub(Topic),
  {ok, #state{mod=Mod, topic=Topic, extra=NewState}}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(Msg, #state{mod=Mod, extra=ExtraState}=State) ->
  NewState=Mod:handle(Msg, ExtraState),
  {noreply, State#state{extra=NewState}}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

