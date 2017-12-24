-module(cmcluster_create_schema).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"host">>}].

key() -> create_schema.

do(#{<<"host">> := Host}, S) ->
    case cmkit:node_for_host(Host) of 
        {ok, Node} -> 
            case cmdb:create_schema(Node) of
                ok -> 
                    {ok, create_schema, #{}, S};
                {error, E} ->
                    {error, E, S}
            end;
        _ ->
            {error, invalid, S}
    end.
