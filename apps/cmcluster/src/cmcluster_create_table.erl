-module(cmcluster_create_table).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"name">>},
           {text, <<"type">>},
           {list, <<"memory">>},
           {list, <<"disc">>},
           {list, <<"both">>} ].

key() -> create_table.

do(#{ <<"name">> := Name,
      <<"type">> := Type,
      <<"memory">> := MemCopies,
      <<"disc" >> := DiscCopies,
      <<"both">> := BothCopies}, S) ->
    
    case {type_for(Type), cmdb:table_for(Name) } of 
        {{ok, T}, {ok, {N, _, _}}}  -> 
            case {cmkit:hosts_to_nodes(MemCopies),
                  cmkit:hosts_to_nodes(DiscCopies),
                  cmkit:hosts_to_nodes(BothCopies)} of 
                {{ok, MemNodes},
                 {ok, DiscNodes},
                 {ok, BothNodes}} -> 
                    case cmdb:c(N, T, [{disc, DiscNodes}, 
                                       {memory, MemNodes}, 
                                       {both, BothNodes}]) of 
                        ok -> 
                            {ok, create_table, #{}, S};
                        {error, exists} ->
                            {error, conflict, S};
                        {error, E} ->
                            {error, E, S}
                    end;
                Other -> 
                    cmkit:log({create_table, replicas, Other }),
                    {error, invalid, S}
            end;
        Other ->
            cmkit:log({create_table, basic, Other }),
            {error, invalid, S}
    end.

type_for(<<"set">>) -> {ok,  set};
type_for(<<"bag">>) -> {ok, bag};
type_for(<<"ordered_set">>) -> {ok, ordered_set};
type_for(_) -> {error, undefined}.
