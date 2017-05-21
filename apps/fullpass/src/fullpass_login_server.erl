-module(fullpass_login_server).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}, 1500}.

handle_call(_, _From, S) ->
  {reply, ok, S}.

handle_cast(_, State) ->
  {noreply, State}.

handle_info(timeout, State) ->
  Workers = erlang:system_info(schedulers_online),
  fullpass_login_worker_sup:start_children(Workers),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

