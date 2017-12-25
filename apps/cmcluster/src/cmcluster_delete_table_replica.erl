-module(cmcluster_delete_table_replica).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"table">>},
           {text, <<"host">>}].

key() -> delete_table_replica.

do(#{ <<"table">> := Table,
      <<"host">> := Host}, S) ->
    case cmdb:table_for(Table) of 
        {ok, {Tab, _, _}}  -> 
            case cmcluster:delete_table_replica(Host, Tab) of
                ok -> 
                    {ok, delete_table_replica, #{}, S};
                {error, E} ->
                    {error, E, S}
            end;
        _ ->
            {error, invalid, S}
    end.
