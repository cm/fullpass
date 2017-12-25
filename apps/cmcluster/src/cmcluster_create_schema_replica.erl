-module(cmcluster_create_schema_replica).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"host">>},
           {text, <<"peer">>}].

key() -> create_schema_replica.

do(#{ <<"host">> := Host, 
      <<"peer">> := Peer }, S) ->
    case cmcluster:create_schema_replica(Host, Peer) of
        ok -> 
            {ok, create_schema_replica, #{}, S};
        {error, E} ->
            {error, E, S}
    end.
