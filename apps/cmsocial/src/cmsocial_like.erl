-module(cmsocial_like).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"id">>},
           {text, <<"type">>}].

key() -> like.

do(_, S) when map_size(S) == 0 ->
    {error, forbidden, S};

do(#{<<"type">> := Type, <<"id">> := Id}, #{id := Uid}=S) ->
    case cmdb:b([{u, likes, Uid, likes, Id},
                 {u, likes, Id, liked_by, Uid}]) of
        ok -> 
            Like = #{ user => Uid,
                      id => Id,
                      type => Type}, 
            {ok, like, Like, S};
        {error, E} ->
            {error, E, S}
    end.
