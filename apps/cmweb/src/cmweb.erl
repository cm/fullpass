-module(cmweb).
-export([all/0, behaviour_info/1]).

behaviour_info(callbacks) ->
  [{spec, 0}, {key, 0}].

all() ->
    [M || M <-erlang:loaded(), cmkit:implements(M, behaviour_info(callbacks))].


