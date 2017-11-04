-module(cmsocial_group_rename).
-export([spec/0, key/0, do/2]).

spec() -> 
    [{text, <<"name">>},
     {text, <<"group">>}].

key() -> group_rename.

do(_, S) when map_size(S) == 0 ->
    {error, forbidden, S};

do(#{<<"name">> := Name, <<"group">> := Gid},  #{id:= Uid}=S) ->
    case cmdb:r(groups, Gid, is) of 
        {ok, #{owner := Uid, name := Name}=G} ->
            {ok, group, G, S};
        {ok, #{owner := Uid, name := OldName}=G} -> 
            G2 = maps:put(name, Name, G),
            case cmdb:b([{d, names, {cmkit:lower_bin(OldName), Uid}, group, Gid},
                         {i, names, {cmkit:lower_bin(Name), Uid}, group, Gid},
                         {u, groups, Gid, is, G2}]) of
                ok -> 
                    {ok, group, G2, S};
                {aborted, _} -> 
                    {error, conflict, S};
                {error, E} -> 
                    {error, E}
            end;
        {ok, Other} ->
            cmkit:log({other, Other}),
            {error, forbidden, S};
        not_found ->
            {error, not_found, S};
        {error, E} -> 
            {error, E, S}
    end.
