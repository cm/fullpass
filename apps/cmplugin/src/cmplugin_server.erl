-module(cmplugin_server).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).
-record(state, {module, key}).

start_link(Module) ->
  gen_server:start_link(?MODULE, [Module], []).

init([Module]) ->
  cmcluster:sub(nosub),
  K = Module:key(default),
  cmcluster:sub(K),
  {ok, #state{module=Module, key=K}}.

handle_info({nosub, Msg}, #state{module=Module}=State) ->
  case Module:missing(Msg) of
    create ->
      K = Module:key(Msg),
      cmplugin_worker:start(Module, K, Msg),
      {noreply, State};
    {read, K, Initial} ->
      cmplugin_worker:start(Module, K, [Initial, 500]), 
      cmdb:read(K),
      {noreply, State};
    ignore ->
      {noreply, State}
  end;

handle_info(Msg, #state{module=Module, key=K}=D) ->
  io:format("server at key: ~p~n  got msg: ~p~n", [K, Msg]),
  K2 = Module:key(Msg),
  case K2 of
    K ->
      {ok, D2} = Module:data(Msg, D),
      {noreply, D2};
    _ ->
      case cmdb:who(K2) of
        [] -> 
          cmplugin_worker:start(Module, K2, Msg);
        _ ->
          cmdb:cast(K2, Msg)
      end,
      {noreply, D}
  end.

handle_call(_, _, State) ->
  {reply, ok, State}.

handle_cast(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
