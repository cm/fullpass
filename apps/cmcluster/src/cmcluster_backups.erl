-module(cmcluster_backups).
-export([spec/0, key/0, do/2]).

spec() -> [].

key() -> backups.

do(_, S) ->
    case cmdb:l(cluster_backups) of
        {ok, Backups} ->
            {ok, backups, Backups, S};
        {error, E} ->
            {error, E, S}
    end.
