-module(cmsocial_search).
-export([spec/0, key/0, do/2]).

spec() -> 
    [{text, <<"q">>}, 
    {text, <<"type">>}].

key() -> search.

do(_, S) when map_size(S) == 0 ->
    {error, forbidden, S};

do(#{<<"q">> := Q, 
     <<"type">> := <<"people">>}, S) ->
    cmdb:f(users, 
            cmkit:lower_bin(Q),
            [<<"username">>, <<"first">>, <<"last">>],
            self(),
            people),
    {noreply, S};

do(_, S) ->
    {error, forbidden, S}.
