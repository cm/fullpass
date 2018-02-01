-module(cmdb).
-export([
         load_csv/2,
         write/2,
         write/3,
         read/2,
         backup/1
        ]).

load_csv(Filename, Fun) -> 
    cmcsv:parse(Filename, 500, fun(Lines) -> 
        lists:foreach(fun(L) ->
            {K, V} = Fun(L),
            write(K, V)
        end, Lines)
    end).

write(Db, Pairs) ->
    gen_statem:call({Db, node()}, {put, Pairs}).

write(Db, K, V) ->
    gen_statem:call({Db, node()}, {put, K, V}).

read(Db, K) ->
    gen_statem:call({Db, node()}, {get, K}).

backup(Db) -> 
    gen_statem:call({Db, node()}, backup).

