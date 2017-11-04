-module(cmsocial_login).
-behaviour(cmweb).
-export([spec/0, key/0, do/2]).

spec() ->
    [{text, <<"username">>}, 
     {text, <<"password">>}].

key() -> login.

do(#{<<"username">> := U,
     <<"password">> := P}, S) when map_size(S) == 0 ->
    case cmdb:s(names, U, user) of
        {ok, [Id]} ->
            case cmdb:r(passwords, Id) of
                {ok, P} ->
                    case cmdb:r(users, Id) of
                        {ok, Profile} ->
                            case cmdb:u(connections, Id, has, self()) of
                                ok -> 
                                    {ok, profile, Profile, Profile};
                                conflict ->
                                    {error, conflict, S};
                                {error, E} ->
                                    {error, E, S}
                            end;
                        not_found ->
                            {error, not_found, S};
                        {error, E} ->
                            {error, E, S}
                    end;
                {ok, _} ->
                    {error, not_found, S};
                not_found -> 
                    {error, not_found, S};
                {error, E} ->
                    {error, E, S}
            end;
        not_found -> 
            {error, not_found, S};
        {error, E} ->
            {error, E, S}
    end;

do(_, #{id := _}=S) ->
    {error, forbidden, S}.
