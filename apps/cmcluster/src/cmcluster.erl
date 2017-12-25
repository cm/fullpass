-module(cmcluster).
-export([drop_schema/1,
        create_schema/1,
        create_table_replica/4,
        delete_table_replica/2,
        create_schema_replica/2]).

drop_schema(Host) ->
    do_in_host(Host, drop_schema).

create_schema(Host) ->
    do_in_host(Host, create_schema).

create_table_replica(Host, Tab, TargetHost, Media) ->
    case cmkit:node_for_host(TargetHost) of
        {ok, TargetNode}  ->
            do_in_host(Host, {create_table_replica, TargetNode, Tab, Media});
        _ ->
            {error, invalid}
    end.

delete_table_replica(Host, Tab) ->
    do_in_host(Host, {delete_table_replica, Tab}).

create_schema_replica(Host, TargetHost) ->
    case cmkit:node_for_host(TargetHost) of
        {ok, TargetNode}  ->
            do_in_host(Host, {create_schema_replica, TargetNode});
        {error, E} ->
            {error, E}
    end.

do_in_host(Host, Op) ->
    case cmkit:node_for_host(Host) of
        {ok, Node} ->
            gen_statem:call({cmcluster_server, Node}, Op);
        {error, E} ->
            {error, E}
    end.
