-module(cmcluster_events).
-export([spec/0, key/0, do/2]).

spec() -> [].

key() -> cluster_events.

do(_, S) ->
    case cmcluster:log() of
        {ok, Events} ->
            {ok, cluster_events, Events, S};
        {error, E} ->
            {error, E, S}
    end.
