-module(cmpresence).
-export([cmdb_tables/0,
        update/3]).

cmdb_tables() ->
     [{presence,set,ram_copies}].

update(Uid, Gid, App) ->
    cmpresence_sup:update(Uid, Gid, App).
