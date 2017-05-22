-module(cmkit).
-export([config/2, err/1, fmt/2, jsond/1, now/0, uuid/0, ret/1, child_spec/2, child_spec/3, child_spec/4, parse/2]).

config(Key, App) ->
  case application:get_env(App, Key) of
    undefined -> undefined;
    {ok, Values} ->
      case Values of 
        [_|_] -> 
          case proplists:get_value(env(), Values) of
            undefined -> proplists:get_value(default, Values);
            V -> V
          end;
        V -> V
      end
  end.

env() ->
  case os:getenv("CMENV", "dev") of
    "prod" -> prod;
    _ -> dev
  end.

jsond(Bin) ->
    try jiffy:decode(Bin, [return_maps])
    catch
      _:_ -> kit:err(<<"Invalid Json">>)
    end.

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
  {Id, {M, start_link, Args}, permanent, 5000, Type, [M]}.

parse(Spec, Json) ->
  parse_input(Spec, [], Json).

parse_input([{Type, K} |Rest], SoFar, Json) ->
  case parse_value(Type, K, Json) of
    invalid -> {errors, #{invalid => K}};
    missing -> {errors, #{missing => K}};
    V -> parse_input(Rest, [V|SoFar], Json)
  end;

parse_input([{Type, K, Default} |Rest], SoFar, Json) ->
  case parse_value(Type, K, Json) of
    invalid -> {errors, #{invalid => K}};
    missing -> parse_input(Rest, [Default|SoFar], Json);
    V -> parse_input(Rest, [V|SoFar], Json)
  end;
  
parse_input([], SoFar, _) -> {ok, lists:reverse(SoFar)}.

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
    V when is_binary(V) -> V;
    _ -> invalid
  end.
