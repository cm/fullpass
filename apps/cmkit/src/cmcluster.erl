-module(cmcluster).
-export([
  sub/1, cmd/1, event/1, query/1, err/2, dispatch/2
]).

sub(T) ->
  ebus:sub(self(), T).

cmd({T, Args}) ->
  dispatch(cmd, {T, Args, self()}).
  
query({T, Args}) ->
  dispatch(query, {T, Args, self()}).

event({T, Args}) ->
  event({T, Args, self()});

event({_, _, _}=E) ->
  dispatch(event, E).

err(Msg, Reason) ->
  dispatch(alarm, {error, Reason, Msg}).

dispatch(T, Msg) ->
  try 
    ebus:dispatch(T, Msg)
  catch
    _:no_subscribers_available -> 
      dispatch(warn, {no_subscribers, T}, false)
  end.

dispatch(T, Msg, false) ->
  try 
    ebus:dispatch(T, Msg)
  catch
    _:no_subscribers_available -> 
      io:format("No subcribers available for topic ~p, args: ~p~n", [T, Msg])
  end.
  

