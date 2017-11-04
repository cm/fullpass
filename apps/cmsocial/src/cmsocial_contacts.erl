-module(cmsocial_contacts).
-export([spec/0, key/0, do/2]).

spec() -> [].

key() -> contacts.

do(_, S) when map_size(S) == 0 ->
    {error, forbidden, S};

do(_, #{id:=Id}=S) ->
    case cmdb:s(groups, Id, belongs_to) of
        {ok, []} ->
            {noreply, S};
        {ok, Gids} ->
            All = [ cmdb:s(groups, Gid, has_member) || Gid <- Gids],
            All2 = lists:filter(fun({ok, _Uids}) -> true;
                                          (_) -> false
                                       end, All),
            All3 = lists:map(fun({ok, Uids}) -> 
                                     Uids
                             end, All2),
            All4 = lists:flatten(All3),
            All5 = lists:filter(fun(Uid) when Uid =:= Id -> false;
                                   (_) -> true
                                end, All4),
            All6 = sets:from_list(All5),
            All7 = sets:to_list(All6),
            lists:foreach(fun(Uid) ->
                            case cmdb:r(users, Uid, is) of
                                {ok, U} ->
                                    self() ! {contact, U};
                                _ ->
                                    cmkit:log({contacts, error, not_found, Uid})
                            end
                          end, All7),
            {noreply, S};
        not_found ->
            {noreply, S};
        {error, E} ->
            {error, E, S}
    end.
