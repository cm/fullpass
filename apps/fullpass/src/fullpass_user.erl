-module(fullpass_user).
-behaviour(cmplugin).
-export([init/1, data/2, key/1, missing/1]).
-record(data, {id, profile}).

key(default) ->
  profile;

key({profile, [#{<<"id">> := Id}, _]}) ->
  {profile, Id}.

init({profile, [#{<<"id">> := Id}=P, Conn]}) ->
  new_session(P, Conn),
  {ok, #data{id=Id, profile=P}}.

data({profile, [#{<<"id">> := Id}=P, Conn]}, 
       #data{id=Id}=D) ->
  % TODO: notify all existings sessions
  % in case the profile info (name, picture) has
  % changed since their last login
  new_session(P, Conn),
  {ok, D#data{profile=P}}.

missing(_) ->
  ignore.

new_session(P, Conn) ->
  Sid = cmkit:uuid(),
  cmdb:write({session, Sid}, [Sid, 
                              calendar:universal_time(),
                              P,
                              Conn]).

