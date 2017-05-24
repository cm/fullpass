-module(fullpass_profile).
-behaviour(cmaggregate).
-export([mode/0, init/1, handle/2, topic/0, topic/1]).
-record(data, {profile}).

mode() -> 
  one_worker.

topic() ->
  profile.

topic({profile, #{<<"id">> := Id}, _}) ->
  {profile, Id}.

init(_) -> 
  #data{}.

handle({profile, P, _}, Data) ->
  Data#data{profile=P}.
