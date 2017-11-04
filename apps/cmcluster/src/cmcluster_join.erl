-module(cmcluster_join).
-export([spec/0, key/0, do/2]).

spec() -> [{text, node}].

key() -> cluster_join.

do(#{ node := Node }, S) ->
    case cmcluster_server:join(Node) of
        {ok, Nodes} ->
            {ok, cluster_join, Nodes, S};
        {error, E} ->
            {error, E, S}
    end.
