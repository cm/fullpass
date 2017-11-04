-module(cmsocial_logout).
-behaviour(cmweb).
-export([spec/0, key/0, do/2]).

spec() -> [].

key() -> logout.

do(_, #{id := Id}=S) ->
    case cmdb:d(connections, Id, has, self()) of
        ok -> 
            {ok, #{}};
        {error, E} ->
            {error, E, S}
    end;

do(_, S) when map_size(S) == 0 ->
    {error, forbidden, S}.
