-module(cmsource).
-behaviour(gen_server).
-export([start_link/2, start_link/3, start_link/4]).
-export([behaviour_info/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {log, module, mode}).

behaviour_info(callbacks) ->
  [{handle, 1}, {handle, 2}, {mode, 0}].

start_link(Mod, LogName) ->
  gen_server:start_link(?MODULE, [Mod, LogName], []).

start_link(Mod, LogName, Topic) ->
  gen_server:start_link(?MODULE, [Mod, LogName, Topic], []).

start_link(Mod, LogName, Topic, Alias) ->
  gen_server:start_link(?MODULE, [Mod, LogName, Topic, Alias], []).

init([Mod, LogName]) ->
  init([Mod, LogName, LogName]);

init([Module, LogName, Topic]) ->
  init([Module, LogName, Topic, LogName]);

init([Module, LogName, Topic, Alias]) ->
  Mode = Module:mode(),
  cmcluster:sub(Topic),
  {ok, Log} = open_log(Mode, LogName, Alias),
  {ok, #state{log=Log, module=Module, mode=Mode}}.

open_log(Mode, Name, Alias) ->
  Opts = log_opts(Name, Alias, Mode),
  case disk_log:open(Opts) of
    {error, E} -> {error, E};
    {ok, Log} -> {ok, Log};
    {repaired, Log, _, _} -> {ok, Log}
  end.

log_opts(Name, Alias, Mode) ->
  [ {name,  Alias}
    , {file,   log_file(Name)}
    , {type,   halt}
    , {format, internal}
    , {mode,   log_mode(Mode)}
  ].

log_mode(Mode) ->
  case Mode of 
    write -> read_write;
    read -> read_only
  end.

log_file(Name) ->
  Dir = cmkit:config(dir, cmsource),
  Filename = atom_to_list(Name) ++ ".log",
  filename:join([Dir, Filename]).

write(Log, {T, Args}) ->
  ok = disk_log:log(Log, {T, Args}),
  ok.

read(Log, T) ->
  {ok, Terms} = filter_log(Log, fun({Topic, _}) ->
                                Topic == T
                                end),
  Terms.

filter_log(Log, FilterFun) ->
  filter_log(Log, start, [], FilterFun).

filter_log(Log, Cont, SoFar, FilterFun) ->
  case disk_log:chunk(Log, Cont) of 
    {error, Rsn} ->
      io:format("error while reading log: ~p, reason: ~p~n", [Log, Rsn]),
      {ok, SoFar};
    {Cont2, Terms} ->
      filter_log(Log, Cont2, aggregate(Terms, SoFar, FilterFun), FilterFun); 
    {Cont2, Terms, _} ->
      filter_log(Log, Cont2, aggregate(Terms, SoFar, FilterFun), FilterFun); 
    eof ->
      {ok, SoFar} 
  end.

aggregate(Terms, SoFar, FilterFun) ->
  SoFar ++ lists:filter(FilterFun, Terms).

close(Log) ->
  disk_log:close(Log).

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, S) ->
  {noreply, S};

handle_info({_, Args, _}=Msg, #state{log=File, mode=read, module=Module}=S) ->
  Data = read(File, Args),
  Module:handle(Msg, Data),
  {noreply, S};

handle_info({T, Args, _}=Msg, #state{log=Log, mode=write, module=Module}=S) ->
  ok = write(Log, {T, Args}),
  Module:handle(Msg),
  {noreply, S}.

terminate(_Reason, #state{log=Log}) ->
  close(Log),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

