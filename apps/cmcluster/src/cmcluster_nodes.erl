-module(cmcluster_nodes).
-export([spec/0, key/0, do/2]).

spec() -> [].

key() -> cluster_nodes.

do(_, S) ->
    case cmcluster_server:all_nodes() of
        {ok, Nodes} ->
            {ok, cluster_nodes, Nodes, S};
        {error, E} ->
            {error, E, S}
    end.
