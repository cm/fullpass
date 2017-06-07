-module(cmdb).
-export([sub/1, 
         broadcast/2, 
         cast/2, 
         write/2,
         read/1,
         who/1]).

sub(K) ->
  ebus:sub(self(), K).

broadcast(K, V) ->
  ebus:pub(K, {K, V}).

cast(K, V) ->
  try
    ebus:dispatch(K, {K, V})
  catch
    _:no_subscribers_available ->
      try 
        ebus:pub(nosub, {nosub, {K, V}}) 
      catch
        _:no_subscribers_available ->
          io:format("Nobody listening for missing events~n  for key:~p~n  and value: ~p~n", [K, V])
      end
  end.

write(K, V) ->
  cmdb_fsm:write(K, V).

read(K) ->
  cmdb_fsm:read(K).

who(K) ->
  ebus:subscribers(K).
