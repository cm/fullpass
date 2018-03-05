-module(cmweb).
-export([
         all/1, 
         behaviour_info/1
        ]).

behaviour_info(callbacks) ->
  [{apps, 0}, {spec, 0}, {key, 0}].

all(App) ->
    [M || M <-erlang:loaded(), in_app(App, M) ].

in_app(App, M) ->
    cmkit:implements(M, behaviour_info(callbacks))
    andalso  (M:apps() =:= all 
              orelse lists:member(App, M:apps())).
