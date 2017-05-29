-module(cmcluster).
-export([
  pub/2, sub/1, cmd/1, event/1, query/1, err/2, dispatch/2, who/1
]).

pub(T, Msg) ->
  ebus:pub(T, Msg).

sub(T) ->
  ebus:sub(self(), T).

who(T) ->
  ebus:subscribers(T).

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
      dispatch(no_subscribers, {T, Msg}, false)
  end.

dispatch(no_subscribers, {T, Msg}, false) ->
  try 
    ebus:pub(no_subscribers, Msg)
  catch
    _:no_subscribers_available -> 
      io:format("No subcribers available for topic: ~p~nfrom event: ~p~n", [T, Msg])
  end.
  

