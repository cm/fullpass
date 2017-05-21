-module(cmsource).
-behaviour(gen_server).
-export([start_link/2]).
-export([behaviour_info/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {log, mod}).

behaviour_info(callbacks) ->
  [{handle, 1}].

start_link(Mod, LogName) ->
  gen_server:start_link(?MODULE, [Mod, LogName], []).

init([Mod, LogName]) ->
  {ok, Log} = open_log(LogName),
  cmcluster:sub(LogName),
  {ok, #state{log=Log, mod=Mod}}.
 
open_log(Name) -> 
  F = logfile(Name),
  file:open(F, [append, binary]).

logfile(Name) ->
  Dir = cmkit:config(dir, cmsource),
  Filename = atom_to_list(Name) ++ ".log",
  filename:join([Dir, Filename]).

write(Log, {T, Args}) ->
  io:format(Log, "~p.~n", [{T, Args, cmkit:now()}]),
  ok.

close(Log) ->
  file:close(Log).

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, S) ->
  {noreply, S};

handle_info({T, Args, _}=Msg, #state{log=Log, mod=Mod}=S) ->
  ok = write(Log, {T, Args}),
  Mod:handle(Msg),
  {noreply, S}.

terminate(_Reason, #state{log=Log}) ->
  close(Log),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

