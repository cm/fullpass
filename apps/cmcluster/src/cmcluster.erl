-module(cmcluster).
-export([cmdb_tables/0]).
-export([drop_schema/1,
        create_schema/1,
        create_table_replica/4,
        delete_table_replica/2,
        create_schema_replica/2,
        stop_db/1,
        start_db/1,
        log/1, 
        log/0, 
        clear_log/0]).


cmdb_tables() -> 
    [{cluster_log, bag, disc_only_copies}].

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
    case cmkit:node_for_host(Host) of
        {ok, Node}  ->
            gen_statem:call({cmcluster_server, Node}, {delete_table_replica, Node, Tab});
        _ ->
            {error, invalid}
    end.

create_schema_replica(Host, TargetHost) ->
    case cmkit:node_for_host(TargetHost) of
        {ok, TargetNode}  ->
            do_in_host(Host, {create_schema_replica, TargetNode});
        {error, E} ->
            {error, E}
    end.

start_db(Host) ->
    do_in_host(Host, start_db).

stop_db(Host) ->
    do_in_host(Host, stop_db).


do_in_host(Host, Op) ->
    case cmkit:node_for_host(Host) of
        {ok, Node} ->
            gen_statem:call({cmcluster_server, Node}, Op);
        {error, E} ->
            {error, E}
    end.

log(E) ->
    {Type, Args} = cmdb:event_for(E),
    Id = cmkit:uuid(),
    Data = #{ id => Id,
              type => Type,
              info => Args,
              date => cmkit:now() },
    cmkit:log({cmdb, E}),
    cmdb:u(cluster_log, Id, Data).

clear_log() ->
    ok.

log() -> 
    cmdb:l(cluster_log).
