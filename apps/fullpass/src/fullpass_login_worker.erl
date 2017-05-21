-module(fullpass_login_worker).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link(Id) ->
  gen_server:start_link(?MODULE, [Id], []).

init([_Id]) ->
  ebus:sub(self(), login),
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({login, {Code}, From}=Cmd, State) ->
  facebook:login(Code, fun(P) ->
    fpcluster:event({profile, P, From})
  end, fun(E) ->
    From ! {error, E},
    fpcluster:err({Cmd, E})
  end),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

