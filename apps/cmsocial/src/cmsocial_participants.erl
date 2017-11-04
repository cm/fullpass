-module(cmsocial_participants).
-export([spec/0, key/0, do/2]).

spec() -> 
     [{text, <<"group">>}].

key() -> participants.

do(_, S) when map_size(S) == 0 ->
    {error, forbidden, S};

do(#{<<"group">> := Gid},  #{id:= Uid}=S) ->
    case cmdb:s(groups, Uid, belongs_to) of 
        {ok, Groups} -> 
            case lists:member(Gid, Groups) of 
                true -> 
                    cmdb:j(groups, Gid, has_member, users, 
                        fun(_) -> true end,
                        fun(User) -> 
                           #{user => User,
                             group => Gid}
                        end,
                        self(), participant),
                    {noreply, S};
                false -> 
                    {error, forbidden, S}
            end;
        not_found ->
            {error, not_found, S};
        {error, E} -> 
            {error, E, S}
    end.
