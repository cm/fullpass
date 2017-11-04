-module(cmsocial_invite_accept).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"invite">>}].

key() -> invite_accept.

do(_, S) when map_size(S) == 0 ->
    {error, forbidden, S};

do(#{<<"invite">> := InvId}, #{id := Id} = S) ->
    case cmdb:r(invites, InvId) of
        {ok, #{status:=new, from:=From, to:=Id, group := Gid}=I} ->
            case {cmdb:r(users, From, is), cmdb:r(groups, Gid, is)} of
                {{ok, Inviter}, {ok, G}} ->
                    I2 = maps:put(status, accepted, I),
                    I3 = maps:merge(I2, #{from =>  Inviter, to => S, group => G}),
                    case cmdb:b([{u, invites, InvId, is, I2},
                                 {u, groups, Gid, has_member, Id},
                                 {u, groups, Id, belongs_to, Gid}]) of
                            ok -> 
                                case cmdb:j(groups, Gid, has_member, users) of
                                    {ok, Participants} ->
                                        [ notifyParticipant(S, U, Gid ) || 
                                            #{ id := Uid }=U <- Participants, Uid =/= Id],
                                        notify(From, invite, I3),
                                        {ok, invite, I3, S};
                                    {error, E} ->
                                        {error, E, S}
                                end;
                            {aborted, _} ->
                                {error, conflict, S};
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

notifyParticipant(#{ id := NewId }=New, #{ id := ExistingId }=Existing, Gid) ->
    notify(NewId, participant, #{ user => Existing, group => Gid }),
    notify(ExistingId, participant, #{ user => New, group => Gid }).


notify(Uid, A, V) ->
    case cmdb:s(connections, Uid, has) of
        {ok, Conns1} ->
            [ C ! {A, V} || C <- Conns1 ],
            ok;
        Other -> 
            cmkit:log({presence, notify, Uid, unexpected, Other}),
            ok
    end.
