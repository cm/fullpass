-module(cmcluster_clear_events).
-export([spec/0, key/0, do/2]).

spec() -> [].

key() -> cluster_clear_events.

do(_, S) ->
    case cmdb:clear(cluster_log) of
        ok -> 
            {ok, cluster_clear_events, S};
        {error, E} ->
            {error, E, S}
    end.
