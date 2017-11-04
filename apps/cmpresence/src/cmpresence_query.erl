-module(cmpresence_query).
-export([spec/0, key/0, do/2]).

spec() -> 
     [{text, <<"group">>},
      {text, <<"user">>},
      {text, <<"application">>}].

key() -> presence_query.

do(_, S) when map_size(S) == 0 ->
    {error, forbidden, S};

do(#{<<"group">> := Gid,
     <<"user">> := Uid2,
     <<"application">> := App},  #{id:= Uid}=S) ->
    case { cmdb:s(groups, Uid2, belongs_to),
            cmdb:s(groups, Uid, belongs_to) } of 
        {{ok, Groups2}, {ok, Groups}} -> 
            case {lists:member(Gid, Groups2), 
                  lists:member(Gid, Groups)} of
                {false, _} -> 
                    {error, forbidden, S};
                {_, false} -> 
                    {error, forbidden, S};
                {true, true} ->
                    case cmdb:r(presence, {}) of
                        {ok, P} ->
                            {reply, presence, P, S};
                        not_found ->
                            Offline = #{user => Uid2, 
                                        group => Gid, 
                                        application => App,
                                        status => offline},
                            {ok, presence, Offline, S}; 
                        {error, E} ->
                            {error, E, S}
                    end
            end;
        {{error, E}, _} -> 
            {error,E, S};
        {_, {error, E}} -> 
            {error,E, S};
        {_, _} -> 
            {error, forbidden, S}
    end.

