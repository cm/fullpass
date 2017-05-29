-module(fullpass_user).
-behaviour(cmaggregate).
-export([mode/0, init/1, handle/2, topic/0, topic/1]).
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
       #data{id=Id, sessions=Sessions}=Data) ->
  % TODO: rework this: register the new session, 
  % but also notify existing sessions with the new profile
  SessionId = cmkit:uuid(),
  cmcluster:sub({session, SessionId}),
  cmcluster:event({session, {SessionId, Id}}),
  Conn ! SessionId,
  Data#data{profile=P, sessions=maps:put(SessionId, Conn, Sessions)};

handle({{session, Id}, _, Conn}, #data{profile=P}=Data) ->
  with_session(Id, Conn, Data, 
               fun(Data2) -> 
                   Data2
               end,
               fun(P, Data2) ->
                   Conn ! P,
                   Data2
               end).

with_session(Id, Conn, #data{sessions=Sessions, profile=P}=Data, Err, Next) ->
  case maps:is_key(Id, Sessions) of
    true ->
      case maps:get(Id, Sessions) of
        Conn ->
          Next(P, Data);
        _ ->
          Sessions2 = maps:put(Id, Conn, Sessions),
          Data2 = Data#data{sessions=Sessions2},
          Next(P, Data2)
      end;
    false ->
      Err(Data)
  end.
