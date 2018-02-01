-module(cmdb).
-export([
         ping/1,
         write/2,
         write/3,
         read/2,
         backup/1,
         restore/2
        ]).

ping(N) ->
    gen_statem:call({cmdb_cloud, N}, ping).

write(Db, Pairs) -> 
    in( node_for(Db), Db, {put, Pairs}).

write(Db, K, V) ->
    in( node_for(Db), Db, {put, K, V}).

read(Db, K) ->
    in( node_for(Db), Db, {get, K}).

backup(Db) -> 
    in( node_for(Db), Db, backup).

restore(Db, Name) -> 
    in( node_for(Db), Db, {restore, Name}).

node_for(Db) -> 
    gen_statem:call({cmdb_cloud, node()}, {node, Db}).

in({ok, N}, Db, Op) ->
    gen_statem:call({Db, N}, Op);

in(_, _, _) ->
    {error, unavailable}.
