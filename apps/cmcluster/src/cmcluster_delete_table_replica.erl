-module(cmcluster_delete_table_replica).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"table">>},
           {text, <<"host">>}].

key() -> delete_table_replica.

do(#{ <<"table">> := Table,
      <<"host">> := Host}, S) ->
    
    case {cmkit:node_for_host(Host), cmdb:table_for(Table) } of 
        {{ok, Node}, {ok, {Tab, _, _}}}  -> 
            case cmdb:drop(Tab, Node) of
                ok -> 
                    {ok, delete_table_replica, #{}, S};
                {error, E} ->
                    {error, E, S}
            end;
        _ ->
            {error, invalid, S}
    end.
