-module(cmcluster_create_table_fully).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"name">>},
           {text, <<"type">>},
           {text, <<"media">>}].

key() -> create_table_fully.

do(#{ <<"name">> := Name,
      <<"type">> := Type,
      <<"media">> := Media }, S) ->
    
    case cmdb:table_for(Name) of
        {ok,  {Tab, _, _}}  ->
            case cmdb:c({Tab, Type, Media, [node()|nodes()]}) of 
                ok -> 
                    {ok, create_table_fully, #{}, S};
                {error, exists} ->
                    {error, conflict, S};
                {error, E} ->
                    {error, E, S}
            end;
        {error, E} -> 
            {error, E, S}
    end.
