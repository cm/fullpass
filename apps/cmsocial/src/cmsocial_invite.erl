-module(cmsocial_invite).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"user">>},
           {text, <<"group">>}].

key() -> invite.

do(_, S) when map_size(S) == 0 ->
    {error, forbidden, S};

do(#{<<"user" >> := From},
   #{id := From}=S) ->
    {error, forbidden, S};


do(#{<<"user">> := To, <<"group">> := Gid}, 
   #{id := From}=S) ->
    case {cmdb:r(users, To, is), cmdb:r(groups, Gid, is)} of
        {{ok, Invitee}, {ok, #{owner := From}=G}} ->
            Id = cmkit:uuid(),
            I = #{ id => Id, from => From, group => Gid, to => To, status => new },
            case cmdb:b([{i, names, {From, To, Gid}, is_invite, Id},
                         {i, invites, Id, is, I},
                         {a, invites_sent, From, has, Id},
                         {a, invites_received, To, has, Id}]) of
                ok -> 
                    I2 = maps:merge(I, #{from =>  S, to => Invitee, group => G}),
                    case cmdb:s(connections, From, has) of
                        {ok, Conns1} ->
                            [ C ! {invite, I2} || C <- Conns1 ],
                            case cmdb:s(connections, To, has) of
                                {ok, Conns2} ->
                                    [ C ! {invite, I2} 
                                      || C <- Conns2 ],
                                    {noreply, S};
                                _ ->
                                    {noreply, S}
                            end;
                        _ ->
                            {noreply, S}
                    end;
                {aborted, _ } -> 
                    {error, conflict,S};
                conflict ->
                    {error, conflict,S};
                {error, E} ->
                    {error, E, S}
            end;
        {{ok, _}, {ok, _}} ->
            {error, forbidden, S};
        {not_found, _} ->
            {error, not_found, S};
        {_, not_found} ->
            {error, not_found, S};
        {{error, E}, _} ->
            {error, E, S};
        {_, {error, E}} ->
            {error, E, S}
    end.
