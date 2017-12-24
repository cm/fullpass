-module(cmcluster_delete_schema).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"host">>}].

key() -> delete_schema.

do(#{<<"host">> := Host}, S) ->
    case cmcluster:drop_schema(Host) of
        ok -> 
            {ok, delete_schema, #{}, S};
        {error, E} ->
            {error, E, S}
    end.
