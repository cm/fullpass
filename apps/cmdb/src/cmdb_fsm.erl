-module(cmdb_fsm).
-behaviour(gen_fsm).
-export([init/1, code_change/4, handle_event/3, handle_info/3,
         handle_sync_event/4, terminate/3]).
-export([start_link/4, start_link/5]).
-export([write/2, read/1]).
-export([prepare/2, execute/2, waiting/2]).
-record(data, {k, cmd, n, id, replies, preflist}).

write(K, V) ->
  cmdb_fsm_sup:start([write,  K, V]).

read(K) ->
  cmdb_fsm_sup:start([read, K]).

start_link(R, W, A, K, V) ->
  gen_fsm:start_link(?MODULE, [R, W, A, K, V], []).

start_link(R, W, A, K) ->
  gen_fsm:start_link(?MODULE, [R, W, A, K], []).

init([_R, W, write, K, V]) ->
  Id = make_ref(),
  Cmd = {write, Id, K, V},
  {ok, prepare, #data{cmd=Cmd, n=W, id=Id, k=K}, 0};

init([R, _W, read, K]) ->
  Id = make_ref(),
  Cmd = {read, Id, K},
  {ok, prepare, #data{cmd=Cmd, n=R, id=Id, k=K}, 0}.

prepare(timeout, #data{k=K, n=N}=D) ->
  DocIdx = chash:key_of(K),
  %DocIdx = riak_core_util:chash_key(K),
  L = riak_core_apl:get_apl(DocIdx, N, cmdb),
  {next_state, execute, D#data{preflist=L}, 0}.

execute(timeout, #data{cmd=Cmd, preflist=L}=D) ->
  riak_core_vnode_master:command(L, 
                                 Cmd,
                                 {fsm, undefined, self()},
                                 cmdb_vnode_master),
  {next_state, waiting, D#data{replies=[]}}.

waiting({ok, Id, V}, #data{k=K, id=Id, n=N, replies=R}=D) ->
  R2 = [V|R], %TODO: resolve conflicts!
  D2 = D#data{replies=R2},
  case length(R2) of
    N ->
      cmdb:dispatch(K, V),
      {stop, normal, D2};
    _ ->
      {next_state, waiting, D2}
  end.

handle_info(_Info, _StateName, StateData) ->
  {stop, badmsg, StateData}.

handle_event(_Event, _StateName, StateData) ->
  {stop, badmsg, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
  {stop, badmsg, StateData}.

code_change(_OldVsn, StateName, State, _Extra) -> 
  {ok, StateName, State}.

terminate(_Reason, _SN, _SD) -> 
  ok.
