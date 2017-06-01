-module(fullpass_session).
-behaviour(cmaggregate).
-export([mode/0, init/1, handle/2, topic/0, topic/1, missing/1, timeout/1]).
-record(data, {id, profile, conn}).

mode() ->
  one_worker.

topic() ->
  session.

topic({{session, Id},  {Id, _P, _Conn}, _}) ->
  {session, Id}.

init({session, Id}) ->
  #data{id=Id, conn=none}.

handle({{session, Id}, {Id, P, Conn}, _}, #data{id=Id, conn=none}=Data) ->
  Conn ! Id,
  Data#data{profile=P, conn=Conn};

handle({{session, Id}, {Id, P, _}}, #data{id=Id, conn=Conn}=Data) ->
  Conn ! P,
  Data#data{profile=P};

handle({{session, Id}, none, Conn}, #data{id=Id, profile=P}=Data) ->
  Conn ! P,
  Data#data{conn=Conn};

handle({{session, Id}, Conn}, #data{id=Id, conn=none}=Data) ->
  Data#data{conn=Conn}.

missing({{session, _}=T, none, Conn}) ->
  {replay, {T, {T, Conn}}};

missing({{session, _}, _, _Conn}) -> 
  allocate;

missing(_) -> 
  ignore.

timeout(Data) ->
  {stop, Data}.
