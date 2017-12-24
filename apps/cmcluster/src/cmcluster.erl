-module(cmcluster).
-export([drop_schema/1,
        create_schema/1]).

drop_schema(Host) ->
    do_in_host(Host, drop_schema).

create_schema(Host) ->
    do_in_host(Host, create_schema).

do_in_host(Host, Op) ->
    case cmkit:node_for_host(Host) of
        {ok, Node} ->
            gen_statem:call({cmcluster_server, Node}, Op);
        _ ->
            {error, invalid}
    end.
