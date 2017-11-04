-module(cmsocial_invites).
-export([spec/0, key/0, do/2]).

spec() -> [].

key() -> invites.

do(_, S) when map_size(S) == 0 ->
    {error, forbidden, S};

do(_, #{id := Id}=S) ->
    FilterFun = fun is_new_invite/1,
    MapFun = fun(I) ->
                  case {cmdb:r(users, maps:get(from, I), is), 
                        cmdb:r(users, maps:get(to, I), is),
                        cmdb:r(groups, maps:get(group, I), is)} of
                      {{ok, From}, {ok, To}, {ok, G}} ->
                          maps:merge(I, #{ from => From,
                                           to => To,
                                           group => G});
                      _ ->
                          I
                  end

          end,
    cmdb:j(invites_received, Id, has, invites, 
           FilterFun, MapFun, 
           self(), invite),
    {noreply, S}.

is_new_invite(I) ->
    cmkit:log({invites, checking_status, I}),
    case is_map(I) of
        true ->
            case maps:get(status,I) of
                new -> true;
                _ -> false
            end;
        false -> false
    end.
