-module(cmcluster_create_table_replica).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"table">>},
           {text, <<"host">>},
           {text, <<"peer">>},
           {text, <<"media">>} ].

key() -> create_table_replica.

do(#{ <<"table">> := Table,
      <<"host">> := Host, 
      <<"peer">> := Peer,
      <<"media">> := Media }, S) ->
    case cmdb:table_for(Table) of 
         {ok, {Tab, _, _}}  -> 
            case table_type_for(Media) of 
                {ok, Type} -> 
                    case cmcluster:create_table_replica(Host, Tab, Peer, Type) of
                        ok -> 
                            {ok, create_table_replica, #{}, S};
                        {error, E} ->
                            {error, E, S}
                    end;
                _ -> 
                    {error, invalid, S}
            end;
        _ ->
            {error, invalid, S}
    end.


table_type_for(<<"both">>) -> {ok, both};
table_type_for(<<"disc">>) -> {ok, disc};
table_type_for(<<"memory">>) -> {ok, memory};
table_type_for(_) -> {error, undef}.


