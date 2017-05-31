-module(cmsource).
-behaviour(gen_server).
-export([start_link/2, start_link/3]).
-export([behaviour_info/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {log, module, mode}).

behaviour_info(callbacks) ->
  [{handle, 1}, {handle, 2}, {mode, 0}].

start_link(Mod, LogName) ->
  gen_server:start_link(?MODULE, [Mod, LogName], []).

start_link(Mod, LogName, Topic) ->
  gen_server:start_link(?MODULE, [Mod, LogName, Topic], []).

init([Mod, LogName]) ->
  init([Mod, LogName, LogName]);

init([Module, LogName, Topic]) ->
  Mode = Module:mode(),
  cmcluster:sub(Topic),
  case Mode of
    write ->
      {ok, Log} = open_log(Mode, LogName),
      {ok, #state{log=Log, module=Module, mode=Mode}};
    read ->
      {ok, #state{log=logfile(LogName), module=Module, mode=Mode}}
  end.

open_log(Mode, Name) -> 
  %file:open(logfile(Name), open_mode(Mode)).

open_mode(Mode) ->
  case Mode of
    write -> [append, binary];
    read -> [read]
  end.

disk_log_opts(Name) ->
  [ {name,   Name}
    , {file,   log_file(Name)}
    , {type,   halt}
    , {format, internal}
    , {mode,   read_write}
  ].

%disk_log_open(File)     -> disk_log:open(disk_log_opts(File)).
%disk_log_write(Log, T)  -> ok = disk_log:blog(Log, term_to_binary(T)).
%disk_log_swrite(Log, T) -> disk_log_write(Log, T), ok = disk_log:sync(Log).
%disk_log_close(Log)     -> disk_log:close(Log).



log_file(Name) ->
  Dir = cmkit:config(dir, cmsource),
  Filename = atom_to_list(Name) ++ ".log",
  filename:join([Dir, Filename]).

write(Log, {T, Args}) ->
  io:format(Log, "~p.~n", [{T, Args, cmkit:now()}]),
  ok.

read(Log, T) ->
  io:format("reading file: ~p~n", [Log]),
  {ok, Terms} = file:consult(Log),
  lists:filter(fun({Topic, _, _}) ->
                Topic == T
               end, Terms).

close(Log) ->
  file:close(Log).

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, S) ->
  {noreply, S};

handle_info({T, _, _}=Msg, #state{log=File, mode=read, module=Module}=S) ->
  Data = read(File, T),
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

