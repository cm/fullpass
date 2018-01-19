-module(cmstore).
-export([
         load_csv/2,
         write/1,
         write/2,
         read/1]).

load_csv(Filename, Fun) -> 
    cmcsv:parse(Filename, 500, fun(Lines) -> 
        lists:foreach(fun(L) ->
            {K, V} = Fun(L),
            write(K, V)
        end, Lines)
    end).

write(Kvs) ->
    gen_statem:call({cmstore_db, node()}, {put, Kvs}).

write(K, V) ->
    gen_statem:call({cmstore_db, node()}, {put, K, V}).

read(K) ->
    gen_statem:call({cmstore_db, node()}, {get, K}).
