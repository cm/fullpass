-module(cmpresence_update).
-export([spec/0, key/0, do/2]).

spec() -> 
     [{text, <<"group">>},
      {text, <<"application">>},
      {text, <<"status">>}].

key() -> presence_update.

do(_, S) when map_size(S) == 0 ->
    {error, forbidden, S};

do(#{<<"group">> := Gid, 
     <<"application">> := App,
     <<"status">> := _Status},  #{id:= Uid}=S) ->
    case cmdb:s(groups, Uid, belongs_to) of 
        {ok, Groups} -> 
            case lists:member(Gid, Groups) of
                false -> 
                    {error, forbidden, S};
                true ->
                    cmpresence_sup:update(Uid, Gid, App),
                    {noreply, S}
            end;
        not_found ->
            {error, not_found, S};
        {error, E} -> 
            {error, E, S}
    end.

