-module(cmsocial_invite_ack).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"invite">>}].

key() -> invite_ack.

do(_, S) when map_size(S) == 0 ->
    {error, forbidden, S};

do(#{<<"invite">> := InvId}, #{id := Id} = S) ->
    case cmdb:r(invites, InvId) of
        {ok, #{status:=Status, from:=Id, from := To}=I} when Status =/= new ->
            case cmdb:r(users, To) of
                {ok, Invitee} ->
                    I2 = maps:put(acknowledged, true, I),
                    I3 = maps:merge(I2, #{from =>  S, to => Invitee}),
                    case cmdb:u(invites, InvId, I2) of
                            ok -> 
                                {ok, invite, I3, S};
                            {error, E} ->
                                {error, E, S}
                    end;
                _ ->
                    {error, not_found, S}
            end;
        {ok, What} ->
            cmkit:log({unexpected_invite, What}),
            {error, forbidden, S};
        not_found ->
            {error, not_found, S};
        {error, E} ->
            {error, E, S}
    end.
