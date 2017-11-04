-module(cmsocial_groups).
-export([spec/0, key/0, do/2]).

spec() -> 
    [].

key() -> groups.

do(_, S) when map_size(S) == 0 ->
    {error, forbidden, S};

do(_,  #{id:= Uid}=S) ->
    cmdb:j(groups, Uid, belongs_to, groups, self(), group),
    {noreply, S}.
