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
    %cmkit:log({cmplaces, Entries}),
    cmdb:write("places", Entries).


%map(Entries) ->
%    lists:flatmap(fun([C, T, Name, Lat, Lon ]) ->
%        %case country(C) of
%        %    undefined -> [];
%       %     _ ->
%                Key = {Lat, Lon},
%                Info = #{ city => Name, area => undefined, zip => undefined,
%                         country => C, lat => Lat, lon => Lon, elev => undefined },
%                [{ Key, Info}, {T, Key}]
%        %%end
%    end, Entries).


parse([C, _Tokens, Name, _, _, Lat, Lon]) ->
    K = <<Lat/binary, <<":">>/binary, Lon/binary>>,
    Place = #{ city => Name, 
               country => C, 
               lat => Lat, 
               lon => Lon},
    %Indices = [],
    %Indices = [ {T, K} || T <- binary:split(Tokens, <<" ">>, [global])],
    [ {K, Place}]. 


    %%case country(C) of 
    %%    undefined -> [];
    %%    _ ->
    %%        Tokens2 = binary:split(Tokens, <<" ">>, [global]), 
    %%        [[C, T, Name, Lat, Lon] || T <- Tokens2].
    %%end.

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
