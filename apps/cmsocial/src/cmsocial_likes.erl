-module(cmsocial_likes).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"id">>},
           {text, <<"type">>}].

key() -> likes.

do(_, S) when map_size(S) == 0 ->
    {error, forbidden, S};

do(#{<<"id">> := Id, <<"type">> := Type}, S) ->
    cmdb:j(likes, Id, liked_by, users, fun(_) -> true end,
           fun(User) ->
                   #{ id => Id,
                      type => Type,
                      user => User }
           end, self(), like),
    {noreply, S}.
