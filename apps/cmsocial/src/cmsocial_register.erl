-module(cmsocial_register).
-export([spec/0, key/0, do/2]).

spec() ->
    [{text, <<"username">>}, 
     {text, <<"password">>}, 
     {text, <<"first">>}, 
     {text, <<"last">>}].

key() -> register.

do(#{<<"username">> := U}=I, S) when map_size(S) == 0 ->
    Id = cmkit:uuid(),
    case cmdb:i(names, U, user, Id) of 
        conflict ->
            {error, conflict, S};
        ok ->
            {Passwd, I2} = maps:take(<<"password">>, I),
            I3 = maps:put(id, Id, I2),
            case cmdb:b([{i, users, Id, I3},
                         {i, passwords, Id, Passwd},
                         {a, connections, Id, has, self()}]) of 
                ok -> 
                    {ok, profile, I3, I3};
                {error, E} ->
                    {error, E, S}
            end
    end;

do(_, #{id := _}=S) ->
    {error, forbidden, S}.
