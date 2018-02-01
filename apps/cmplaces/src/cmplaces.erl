-module(cmplaces).
-export([cmdb_tables/0, search/1, search/2, load/1]).

cmdb_tables() ->
    lists:flatmap(fun(#{ table := LocationsTab, index := IndexTab }) -> 
                          [{LocationsTab,set,disc_only_copies}, 
                           {IndexTab,bag,disc_only_copies}]
                  end, maps:values(countries())).

load(Filename) ->
    cmcsv:parse(Filename, 500, fun store/1).


search(C, Keyword) ->
    case country(C) of 
        undefined -> [];
        #{ table := LocationsTab, index := IndexTab } ->
            case cmdb:j(IndexTab, Keyword, is, LocationsTab) of
                {ok, Places} -> Places;
                _ -> []
            end
    end.

search(Keyword) ->
    lists:flatmap(fun(C) ->
                          search(C, Keyword)
                  end, maps:keys(countries())).


store(Places) ->
    Entries = lists:flatmap(fun(P) ->
                                    parse(P)
                            end, Places),
    %%cmkit:log({cmplaces, Entries}),
    %lists:foreach(fun({K, V}) ->
                    cmdb:write(places, Entries),
    %              end, Entries),
    {ok, length(Entries)}.


parse([C, Tokens, Name, _, _, Lat, Lon]) ->
    Lat2 = cmkit:bin_to_number(Lat),
    Lon2 = cmkit:bin_to_number(Lon),
    K = {Lat2, Lon2},
    P = #{ city => Name, 
           country => C, 
           lat => Lat2, 
           lon => Lon2},

    Tokens2 = lists:flatmap(fun(T) -> 
                                    binary:split(T, <<"-">>, [global])
                            end, binary:split(Tokens, <<" ">>, [global])),
    Indices = [ {T, K} || T <- Tokens2],
    [ {K, P} | Indices ]. 

countries() -> 
    #{ 
        <<"ad">> => #{ name => <<"Andorra">>,
                      code => <<"ad">>,
                      search_by => <<"andorra">>, 
                      table => places_id_coord,
                      index => places_id_index },
  
        <<"id">> => #{ name => <<"Indonesia">>,
                      code => <<"id">>,
                      search_by => <<"indonesia">>, 
                      table => places_id_coord,
                      index => places_id_index } }.

country(Code) -> 
    case maps:get(Code, countries(), undefined) of
        undefined -> undefined;
        Country -> Country
    end.
