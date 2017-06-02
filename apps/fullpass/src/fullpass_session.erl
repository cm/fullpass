-module(fullpass_session).
-behaviour(cmaggregate).
-export([mode/0, init/1, handle/2, topic/0, topic/1, missing/1, timeout/1]).
-record(data, {id, profile, conn, created}).

mode() ->
  one_worker.

topic() ->
  session.

topic({{session, Id},  {Id, _Created, _P, _Conn}, _}) ->
  {session, Id}.

init({session, Id}) ->
  #data{id=Id, conn=none}.

handle({{session, Id}, {Id, Created, P, Conn}, _}, #data{id=Id, conn=none}=Data) ->
  Conn ! Id,
  {ok, Data#data{profile=P, conn=Conn, created=Created}};

handle({{session, Id}, {Id, Created, P, _}}, #data{id=Id, conn=Conn}=Data) ->
  case expired(Created) of
    false ->
      Conn ! P,
      {ok, Data#data{profile=P}};
    true ->
      Conn ! #{session => expired},
      {stop, Data}
  end;

handle({{session, Id}, none, Conn}, #data{id=Id, profile=P, created=Created}=Data) ->
  case expired(Created) of
    false ->
      Conn ! P,
      {ok, Data#data{conn=Conn}};
    true ->
      Conn ! #{session => expired},
      {stop, Data}
  end;

handle({{session, Id}, Conn}, #data{id=Id, conn=none}=Data) ->
  {ok, Data#data{conn=Conn}}.

missing({{session, _}=T, none, Conn}) ->
  {replay, {T, {T, Conn}}};

missing({{session, _}, _, _Conn}) -> 
  allocate;

missing(_) -> 
  ignore.

timeout(#data{conn=Conn}=Data) ->
  Conn ! #{session => not_found},
  {stop, Data}.

expired(Created) ->
  Mins = cmkit:mins_since(Created),
  TTL = cmkit:config(session_ttl, fullpass),
  Mins > TTL.
