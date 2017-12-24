-module(cmcluster_delete_schema).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"host">>}].

key() -> delete_schema.

do(#{<<"host">> := Host}, S) ->
    case cmkit:node_for_host(Host) of 
        {ok, Node} -> 
            case cmdb:drop_schema(Node) of
                ok -> 
                    {ok, delete_schema, #{}, S};
                {error, E} ->
                    {error, E, S}
            end;
        _ ->
            {error, invalid, S}
    end.
