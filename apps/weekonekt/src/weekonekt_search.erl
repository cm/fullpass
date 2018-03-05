-module(weekonekt_search).
-behaviour(cmweb).
-export([apps/0, spec/0, key/0, do/2]).

apps() -> [weekonekt].

key() -> search.

spec() -> 
    [{text, <<"keyword">>}].

do(#{<<"keyword">>:= Word}, S) ->
    [ send(E) || E <- cmdb:read(weekonekt, {token, Word})],
    {noreply, S}.

send({_, {image, Id}=K}) when is_binary(Id) ->
    case image_for(K) of 
        {ok, Img} -> cmdb:read(weekonekt, K),
            self() ! {image, Img};
        E ->
            cmkit:log({weekonekt, Id, cannot_resolve, E})
    end;

send(_) -> ignore.

image_for(K) ->
    case cmdb:read(weekonekt, K) of 
        Items when is_list(Items) -> first_image(Items);
        E -> E
    end.

first_image([]) -> {error, not_found};
first_image([{ _, #{id := _, 
              title := Title,
              url := _,
              date := {Y, M, D, H, Min, _}}=Item}|_]) -> 
    {ok, Item#{ title := list_to_binary(Title),
                date => #{ year => Y, 
                      month => M,
                      day => D,
                      hour => H,
                      min => Min }}};
first_image([_|Rest]) -> first_image(Rest).
