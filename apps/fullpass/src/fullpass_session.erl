-module(fullpass_session).
-behaviour(cmaggregate).
-export([mode/0, init/1, handle/2, topic/0, topic/1]).
-record(data, {id, profile, conn}).

mode() ->
  one_worker.

topic() ->
  profile.

topic({profile, _, _}) ->
  {session, cmkit:uuid()}.

init({session, Id}) -> 
  #data{id=Id}.

handle({profile, P, Conn}, #data{id=Id}=Data) ->
  Conn ! Id,
  Data#data{profile=P, conn=Conn};

handle({session, Id, Conn}, #data{conn=Conn, profile=P, id=Id}=Data) ->
  Conn ! P,
  Data.


