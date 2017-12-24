-module(cmcluster_create_schema).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"host">>}].

key() -> create_schema.

do(#{<<"host">> := Host}, S) ->
    case cmcluster:create_schema(Host) of
        ok -> 
            {ok, create_schema, #{}, S};
        {error, E} ->
            {error, E, S}
    end.
