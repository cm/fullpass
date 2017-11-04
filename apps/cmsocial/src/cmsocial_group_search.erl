-module(cmsocial_group_search).
-export([spec/0, key/0, do/2]).

spec() -> 
    [{text, <<"name">>}].

key() -> group_search.

do(_, S) when map_size(S) == 0 ->
    {error, forbidden, S};

do(#{<<"name">>:= Name},  #{id:=Uid}=S) ->
    Filter = fun(#{owner := Owner}=G) ->
                     %TODO: filter out groups I belong to
                     cmkit:log({group, G, caller, S}),
                     Owner =:= Uid
             end,
    cmdb:l(names, {cmkit:lower_bin(Name), '_'}, group, Filter, groups, self(), group),
    {noreply, S}.
