-module(fullpass_session).
-behaviour(cmaggregate).
-export([mode/0, init/1, handle/2, topic/0, topic/1, missing/1]).
-record(data, {id, profile, sessions}).

mode() ->
  one_worker.

topic() ->
  session.

topic({session, {Id, _Profile, _Conn}, _}=Msg) ->
  io:format("translating msg ~p~ninto worker topic", [Msg]),
  {session, Id}.

init({session, Id}) ->
  #data{id=Id}.

handle({session, Id, _Profile}, #data{id=Id}=Data) ->
  Data.

missing(Msg) ->
  io:format("missing: ~p~n", [Msg]).
