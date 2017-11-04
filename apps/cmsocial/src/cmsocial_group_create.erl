-module(cmsocial_group_create).
-export([spec/0, key/0, do/2]).

spec() -> 
    [{text, <<"name">>}].

key() -> group_create.

do(_, S) when map_size(S) == 0 ->
    {error, forbidden, S};

do(#{<<"name">> := Name},  #{id:= Uid}=S) ->
    Gid = cmkit:uuid(),
    G = #{ id => Gid, name => Name, owner => Uid },
    case cmdb:b([{i, names, {cmkit:lower_bin(Name), Uid}, group, Gid},
                 {i, groups, Gid, is, G},
                 {u, groups, Gid, owned_by, Uid},
                 {u, groups, Uid, owns, Gid},
                 {u, groups, Gid, has_member, Uid},
                 {u, groups, Uid, belongs_to, Gid}]) of 
        ok -> 
            {ok, group, G, S};
        {aborted, _} ->
            {error, conflict, S};
        conflict -> 
            {error, conflict, S};
        {error, E} -> 
            {error, E, S}
    end.
