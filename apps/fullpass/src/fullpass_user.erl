-module(fullpass_user).
-behaviour(cmaggregate).
-export([mode/0, init/1, handle/2, topic/0, topic/1, missing/1]).
-record(data, {id, profile, sessions}).

mode() ->
  one_worker.

topic() ->
  profile.

topic({profile, #{<<"id">> := Id}, _}) ->
  {profile, Id}.

init({profile, Id}) ->
  #data{id=Id, sessions=#{}}.

handle({profile, #{<<"id">> := Id}=P, Conn}, 
       #data{id=Id, sessions=_Sessions}=Data) ->
  SessionId = cmkit:uuid(),
  T = {session, SessionId},
  Now = calendar:universal_time(),
  Args = {SessionId, Now, P, Conn},
  E = {T, Args},
  cmcluster:event(E),
  {ok, Data#data{profile=P}}.

missing(_) ->
  ignore.
