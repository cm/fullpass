-module(fullpass_session).
-behaviour(cmplugin).
-export([init/1, data/2, key/1, missing/1, timeout/1]).
-record(data, {id, profile, conn, created}).

key(default) -> session;

key({{session, Id}, _}) ->
  {session, Id}.

init({{session, Id}, [Since, P, Conn]}) ->
  Conn ! Id,
  {ok, #data{id=Id, 
             profile=P,
             conn=Conn, 
             created=Since}};

init({{session, Id}, Conn}) ->
  {ok, #data{id=Id,
             conn=Conn}}.

data({{session, Id}, [Since, P, _]}, 
     #data{id=Id, conn=Conn}=Data) ->
  case expired(Since) of
    false ->
      Conn ! P,
      {ok, Data#data{profile=P}};
    true ->
      Conn ! #{session => expired},
      {stop, Data}
  end;

data({{session, Id}, [Conn]}, 
       #data{id=Id, profile=P, created=Since}=Data) ->
  case expired(Since) of
    false ->
      Conn ! P,
      {ok, Data#data{conn=Conn}};
    true ->
      Conn ! #{session => expired},
      {stop, Data}
  end.

missing({{session, _}=K, [Conn]}) ->
  {read, K, {K, Conn}};

missing({{session, _}, [_, _, _]}) ->
  create;

missing(_) -> 
  ignore.

timeout(#data{conn=Conn}=Data) ->
  Conn ! #{session => not_found},
  {stop, Data}.

expired(Created) ->
  Mins = cmkit:mins_since(Created),
  TTL = cmkit:config(session_ttl, fullpass),
  Mins > TTL.
