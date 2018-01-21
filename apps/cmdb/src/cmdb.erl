-module(cmdb).
-export([
         load_csv/2,
         write/2,
         write/3,
         read/2
        ]).

load_csv(Filename, Fun) -> 
    cmcsv:parse(Filename, 500, fun(Lines) -> 
        lists:foreach(fun(L) ->
            {K, V} = Fun(L),
            write(K, V)
        end, Lines)
    end).

write(Ns, Pairs) ->
    gen_statem:call({cmdb_write, node()}, {put, Ns, Pairs}).

write(Ns, K, V) ->
    gen_statem:call({cmdb_write, node()}, {put, Ns, K, V}).

read(Ns, K) ->
    gen_statem:call({cmdb_read, node()}, {get, Ns, K}).
