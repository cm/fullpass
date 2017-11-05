-module(cmkit).
-export([log/1, config/2, config/3, err/1, fmt/2, jsone/1, jsond/1, now/0, uuid/0, ret/1, child_spec/2, child_spec/3, child_spec/4, child_spec/5, parse/2, diff_mins/2, diff_secs/2, mins_since/1, match_map/2, search_map/2, search_map/3, implements/2, lower_bin/1, list_without/2, bin_to_number/1, distinct/1]).

log(Data)->
    io:format("[LOG] ~p~n", [Data]).

config(Key, App) ->
    case application:get_env(App, Key) of
        undefined -> undefined;
        {ok, Val} -> Val
    end.

config(Key, App, Default) ->
    case config(Key, App) of 
        undefined -> Default;
        Val -> Val
    end.

jsond(Bin) ->
    try jsone:decode(Bin, [{object_format, map}])
    catch
      _:_ -> kit:err(<<"Invalid Json">>)
    end.

jsone(Term) ->
    jsone:encode(Term).

err(Reason) ->
  {error, #{reason => Reason}}.

fmt(Format, Args) ->
  erlang:iolist_to_binary(io_lib:format(xmerl_ucs:to_utf8(Format), Args )).

now() ->
  {Mega, Secs, _} = os:timestamp(), 
  Mega*1000000 + Secs.

uuid() ->
  list_to_binary(uuid:to_string(uuid:uuid4())).

ret(R) -> R.

child_spec(M, Type) ->
  child_spec(M, [], Type).

child_spec(M, Args, Type) ->
  child_spec(M, M, Args, Type).

child_spec(Id, M, Args, Type) ->
    child_spec(Id, M, Args, permanent, Type).

child_spec(Id, M, Args, Restart, Type) ->
  {Id, {M, start_link, Args}, Restart, 5000, Type, [M]}.

parse(Spec, Json) ->
  parse_input(Spec, #{}, Json).

parse_input([{Type, K} |Rest], SoFar, Json) ->
  case parse_value(Type, K, Json) of
    invalid -> {errors, #{invalid => K}};
    missing -> {errors, #{missing => K}};
    V -> parse_input(Rest, maps:put(K, V, SoFar), Json)
  end;

parse_input([{Type, K, Default} |Rest], SoFar, Json) ->
  case parse_value(Type, K, Json) of
    invalid -> {errors, #{invalid => K}};
    missing -> parse_input(Rest, maps:put(K, Default, SoFar), Json);
    V -> parse_input(Rest, maps:put(K, V, SoFar), Json)
  end;
  
parse_input([], SoFar, _) -> {ok, SoFar}.

parse_value(Type, [K|Rest], Json) ->
  case maps:is_key(K, Json) of
    false -> missing; 
    true -> 
      parse_value(Type, Rest, maps:get(K, Json))
  end;


parse_value(Type, K, Json) ->
  case maps:is_key(K, Json) of
    false -> missing;
    true ->
      parse_actual(Type, K, Json)
  end.

parse_actual(text, K, Json) ->
  case maps:get(K, Json) of
    <<"">> -> invalid;
    V when is_binary(V) -> V;
    _ -> invalid
  end.

mins_since(T) ->
  diff_mins(T, calendar:universal_time()).

diff_mins(T1, T2) ->
  {D, {H, M, _}} = calendar:time_difference(T1, T2),
  M+60*H+24*60*D.

diff_secs(T1, T2) ->
  60*diff_mins(T1, T2).


match_map([{K, V}|Rem], Map) when is_map(Map) ->
    case maps:is_key(K, Map) of
        false -> false;
        true ->
            case maps:get(K, Map) of 
                V -> 
                    match_map(Rem, Map);
                _ -> false
            end
    end;

match_map([], _) -> true;
match_map(_, _)  -> false.

search_map(Pattern, Fields, Map) when is_map(Map) ->
    search_map(Pattern, maps:with(Fields, Map));

search_map(_, _, _) -> false.

search_map(Pattern, Map) when is_map(Map) ->
    cmkit:log({cmkit, search_map, Pattern, Map}),
    search_list(Pattern, maps:values(Map));

search_map(_,_) -> false.

search_list(_, []) -> false;
search_list(Pattern, [H|T]) when is_binary(H) ->
    case binary:match(lower_bin(H), Pattern) of
        nomatch ->
            search_list(Pattern, T);
        {_, _} -> 
            true
    end;

search_list(Pattern, [_|T]) -> 
    search_list(Pattern, T).


implements(M, Callbacks) ->
    Exports = M:module_info(exports),
    Found = lists:filter(fun(Callback) ->
                lists:member(Callback, Callbacks)
            end, Exports),
    length(Found) == length(Callbacks).


lower_bin(B) ->
    binary:list_to_bin(string:to_lower(binary:bin_to_list(B))).


list_without(Map, List) ->
    list_without(Map, [], List).

list_without(_, Out, []) ->
    lists:reverse(Out);

list_without(#{id := Id}=Map, Out, [#{ id := Id}|Rest]) ->
    list_without(Map, Out, Rest);

list_without(Map, Out, [H|Rest]) ->
    list_without(Map, [H|Out], Rest).

bin_to_number(B) ->
    list_to_number(binary_to_list(B)).

list_to_number(L) ->
    try list_to_float(L)
    catch
        error:badarg ->
            list_to_integer(L)
    end.

distinct(List) ->
    sets:to_list(sets:from_list(List)).
