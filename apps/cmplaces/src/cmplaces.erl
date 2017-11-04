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
    cmdb:b(lists:flatmap(fun(P) ->
                                      map(parse(P)) 
                              end, Places)).

map(Entries) ->
    lists:flatmap(fun([C, T, Name, Lat, Lon ]) ->
        case country(C) of
            undefined -> [];
            #{ table := LocationsTab, index := IndexTab } ->
                Key = {Lat, Lon},
                Info = #{ city => Name, area => undefined, zip => undefined,
                         country => C, lat => Lat, lon => Lon, elev => undefined },
                [{u, LocationsTab, Key, is, Info}, 
                 {u, IndexTab, T, is, Key}]
        end
    end, Entries).


parse([C, Tokens, Name, _, _, Lat, Lon]) ->
    case country(C) of 
        undefined -> [];
        _ ->
            Tokens2 = binary:split(Tokens, <<" ">>, [global]), 
            [[C, T, Name, Lat, Lon] || T <- Tokens2]
    end.

countries() -> 
    #{ <<"id">> => #{ name => <<"Indonesia">>,
                      code => <<"id">>,
                      search_by => <<"indonesia">>, 
                      table => places_id_coord,
                      index => places_id_index } }.

country(Code) -> 
    case maps:get(Code, countries(), undefined) of
        undefined -> undefined;
        Country -> Country
    end.
