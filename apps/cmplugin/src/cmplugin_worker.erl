-module(cmplugin_worker).
-behaviour(gen_server).
-export([start/3, start_link/3, start_link/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {mod, key, data}).


start(Module, K, V) ->
  Sup = cmplugin_worker_sup:registered_name(Module),
  supervisor:start_child(Sup, [K, V]).

start_link(Module, K, V) ->
  gen_server:start_link(?MODULE, [Module, K, V], []).

start_link(Module, K, Initial, Timeout) ->
  gen_server:start_link(?MODULE, [Module, K, Initial, Timeout], []).

init([Module, K, V]) ->
  D = Module:init(K),
  case Module:data(V, D) of
    {ok, D2} ->
      cmdb:sub(K),
      {ok, #state{mod=Module, k=K, data=D2}};
    {stop, D2} ->
      {stop, normal, #state{data=D2}}
  end;

init([Module, K, Initial, Timeout]) ->
  case init([Module, K, Initial]) of
    {ok, State} ->
      {ok, State, Timeout};
    {stop, State} ->
      {stop, normal, State}
  end.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, #state{mod=Module, data=D}=State) ->
  case Module:timeout(D) of
    {ok, D2} ->
      {noreply, State#state{data=D2}};
    {stop, D2} ->
      {stop, normal, State#state{data=D2}}
  end;

handle_info(Msg, #state{mod=Module, data=D}=State) ->
  case Module:data(Msg, D) of 
    {stop, D2} -> 
      {stop, normal, State#state{data=D2}};
    {ok, D2} ->
      {noreply, State#state{data=D2}}
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
