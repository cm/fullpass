-module(cmdb_vnode).
-behaviour(riak_core_vnode).
-export([start_vnode/1,
         init/1,
         handle_command/3,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3,
         delete/1,
         handle_overload_command/3,
         handle_overload_info/2,
         is_empty/1,
         terminate/2
        ]).
-record(data, {partition, table, errors}).

start_vnode(P) ->
  riak_core_vnode_master:get_vnode_pid(P, ?MODULE).

init([P]) ->
  File = db_file(P),
  case dets:open_file(File, []) of
    {ok, File} ->
      {ok, #data{partition=P, table=File}};
    {error, E} ->
      {stop, error, #data{partition=P, errors=[E]}}
  end.

db_file(P) ->
  Dir = cmkit:config(dir, cmdb),
  filename:join([Dir, integer_to_list(P)]).

handle_command(ping, _, #data{partition=P}=S) ->
  {reply, {pong, P}, S};

handle_command({write, Id, K, V}, _, #data{table=T}=D) ->
  case dets:insert(T, {K, V}) of
    {error, E} ->
      {reply, {error, Id, E}, D};
    ok ->
      {reply, {ok, Id, V}, D}
  end;

handle_command({read, Id, K}, _, #data{table=T}=D) ->
  case dets:lookup(T, K) of
    {error, E} ->
      {reply, {error, Id, E}, D};
    [{K, V}] ->
      {reply, {ok, Id, V}, D};
    [] ->
      {reply, {nodata, Id}, D}
  end.

handle_handoff_command(_FoldReq, _, S) ->
  {noreply, S}.

handoff_starting(_TargetNode, S) ->
  {true, S}.

handoff_cancelled(S) ->
  {ok, S}.

handoff_finished(_TargetNode, S) ->
  {ok, S}.

handle_handoff_data(_Data, S) ->
  {reply, ok, S}.

encode_handoff_item(K, V) ->
  term_to_binary({K, V}).

is_empty(S) ->
  {true, S}.

delete(S) ->
  {ok, S}.

handle_coverage(_Req, _KeySpaces, _Sender, S) ->
  {stop, not_implemented, S}.

handle_exit(_Pid, _Reason, S) ->
  {noreply, S}.

handle_overload_command(_Cmd, _Sender, S) ->
  {noreply, S}.

handle_overload_info(_Cmd, _) ->
  ok.

terminate(_Reason, #data{table=T}) ->
  dets:close(T),
  ok.
