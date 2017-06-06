-module(cmplugin_server).
-behaviour(gen_server).
-export([start_link/2]).
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).
-record(state, {module, topic, data}).

start_link(Module, Mode) ->
  gen_server:start_link(?MODULE, [Mode], []).

init([Module, Mode]) ->
  cmcluster:sub(nosub),
  K = Module:key(),
  cmcluster:sub(K),
  {ok, #state{module=Module, topic=T}}.

handle_info({nosub, Msg}, #state{module=Module}=State) ->
  case Module:missing(Msg) of
    create ->
      K = Module:key(Msg),
      cmplugin_worker:start(Module, T, Msg),
      {noreply, State};
    {read, K, Initial} ->
      cmplugin_worker:start(Module, K, [Initial, 500]), 
      cmdb:read(K),
      {noreply, State};
    ignore ->
      {noreply, State}
  end;

handle_info(Msg, #state{module=Module}=State) ->
  T = Module:key(Msg),
  case cmdb:who(T) of
    [] -> 
      cmplugin_worker:start(Module, T, Msg)
    _ ->
      cmdb:cast(T, Msg)
  end,
  {noreply, State}

handle_call(_, _, State) ->
  {reply, ok, State}.

handle_cast(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
