-module(fullpass_session).
-behaviour(cmplugin).
-export([init/1, data/2, key/1, missing/1, timeout/1]).
-record(data, {id, profile, conn, created}).

key(default) -> session;

key({{session, Id}, _}) ->
  {session, Id}.

init([Id, Since, P, Conn]) ->
  Conn ! Id,
  {ok, #data{id=Id, 
             profile=P,
             conn=Conn, 
             created=Since}};

init([Id, Conn]) ->
  {ok, #data{id=Id,
             conn=Conn}}.

data([Id, Since, P, _], #data{id=Id, conn=Conn}=Data) ->
  if_not_expired(Since, Conn, Data, fun() ->
                                  Conn ! P,
                                  {ok, Data#data{profile=P, created=Since}}
                              end);

data([Id, Conn], #data{id=Id, profile=P, created=Since}=Data) ->
  if_not_expired(Since, Conn, Data, fun() ->
                                  Conn ! P,
                                  {ok, Data#data{conn=Conn}}
                              end).
  
missing({{session, _}=K, [_, _]=Msg}) ->
  {read, K, Msg};

missing({{session, _}, [_, _, _, _]}) ->
  create;

missing(_) ->
  ignore.

timeout(#data{conn=Conn}=Data) ->
  Conn ! session_not_found,
  {stop, Data}.

expired(Created) ->
  Mins = cmkit:mins_since(Created),
  TTL = cmkit:config(session_ttl, fullpass),
  Mins > TTL.

if_not_expired(Since, Conn, Data, Next) ->
  case expired(Since) of
    false -> 
      Next();
    true ->
      Conn ! session_expired,
      {stop, Data}
  end.
