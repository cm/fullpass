-module(cmsocial_invite_decline).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"invite">>}].

key() -> invite_decline.

do(_, S) when map_size(S) == 0 ->
    {error, forbidden, S};

do(#{<<"invite">> := InvId}, #{id := Id} = S) ->
    case cmdb:r(invites, InvId) of
        {ok, #{status:=new, from:=From, to:=Id}=I} ->
            case cmdb:r(users, From) of
                {ok, Inviter} ->
                    I2 = maps:put(status, declined, I),
                    I3 = maps:merge(I2, #{from =>  Inviter, to => S}),
                    case cmdb:u(invites, InvId, I2) of
                            ok -> 
                                {ok, invite, I3, S};
                            {error, E} ->
                                {error, E, S}
                    end;
                _ ->
                    {error, not_found, S}
            end;
        {ok, _} ->
            {error, forbidden, S};
        not_found ->
            {error, not_found, S};
        {error, E} ->
            {error, E, S}
    end.
