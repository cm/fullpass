-module(cmdb).
-export([ping/0, 
         sub/1, 
         broadcast/2, 
         dispatch/2, 
         write/2,
         read/1]).

ping() ->
  K = {<<"ping">>, term_to_binary(os:timestamp())},
  send_to_one(K, ping).

sub(K) ->
  ebus:sub(self(), K).

broadcast(K, V) ->
  ebus:pub(K, {K, V}).

dispatch(K, V) ->
  try
    ebus:dispatch(K, {K, V})
  catch
    _:no_subscribers_available ->
      try 
        ebus:pub(missing, {K, V}) 
      catch
        _:no_subscribers_available ->
          io:format("Nobody listening for missing events~n  for key:~p~n  and value: ~p~n", [K, V])
      end
  end.

write(K, V) ->
  cmdb_fsm:write(K, V).

read(K) ->
  cmdb_fsm:read(K).

send_to_one(K, Cmd) ->
  Idx = riak_core_util:chash_key(K),
  PrefList = riak_core_apl:get_primary_apl(Idx, 1, cmdb),
  io:format("Got vnodes: ~p~n  for key: ~p~n  hashed as: ~p~n",
           [PrefList, K, Idx]),
  [{N, _}]=PrefList,
  riak_core_vnode_master:sync_spawn_command(N, Cmd, cmdb_vnode_master).
