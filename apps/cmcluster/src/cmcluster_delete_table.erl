-module(cmcluster_delete_table).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"host">>},
           {text, <<"table">>}].

key() -> cluster_delete_table.

do(#{ <<"host">> := _Host,
      <<"table">> := _Table }, S) ->
    case cmcluster_server:all_nodes() of
        {ok, Nodes} ->
            {ok, cluster_nodes, Nodes, S};
        {error, E} ->
            {error, E, S}
    end.
