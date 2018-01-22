-module(cmdb_util).
-export([
         load_csv/6,
         state/0,
         open/3,
         close/3,
         tail/3,
         it/3,
         it/4,
         write_all/3,
         write/3,
         read/2,
         all_ok/1,
         is_local/1,
         current_nodes/0,
         expected_nodes/0
        ]).

load_csv(Name, Type, Size, Filename, BatchSize, Fun) ->
    {ok, Env, Db} = open(Name, Type, Size),
    cmcsv:parse(Filename, BatchSize, fun(Lines) ->
                                        Pairs= lists:flatmap(Fun, Lines),
                                        write_all(Env, Db, Pairs)
                                     end).

open(Name, Type, Size) -> 
    Opts = opts(Type),
    Workdir = cmkit:config(workdir, cmdb),
    FullDir = string:join([Workdir, Name], "/"),
    {ok, Env} = elmdb:env_open(FullDir, [{map_size, Size}]),
    {ok, Db} = elmdb:db_open(Env, Opts),
    {ok, Env, Db}.

opts(set) -> [create];
opts(bag) -> [create, dup_sort].

close(App, Ns, Env) ->
    Res = elmdb:env_close(Env),
    cmkit:log({cmdb, closed, Ns, App, Res}),
    Res.

write(Db, K, V) ->
    Kbin = erlang:term_to_binary(K),
    Vbin = erlang:term_to_binary(V),
    elmdb:put(Db, Kbin, Vbin).

write_all(Env, Db, Pairs) ->
    {ok, Txn} = elmdb:txn_begin(Env),
    lists:foreach(fun({K, V}) ->
                    elmdb:txn_put(Txn, Db, erlang:term_to_binary(K), 
                                 erlang:term_to_binary(V)
                                 )
                  end, Pairs),
    ok = elmdb:txn_commit(Txn),
    ok.

read(Db, K) ->
    case elmdb:get(Db, erlang:term_to_binary(K)) of
        not_found -> not_found;
        {ok, V} -> {ok, erlang:binary_to_term(V)}
    end.


expected_nodes() -> 
    Sname = erlang:binary_to_list(cmkit:sname()),
    [ erlang:list_to_atom(
        string:join([ Sname,
                      erlang:atom_to_list(H)
                    ], "@")
       ) || H <- net_adm:host_file()].

current_nodes() ->
      ExpectedNodes = expected_nodes(),
      cmkit:intersection(ExpectedNodes, [node()|nodes()]).

is_local(#{ replicas := Hosts}) ->
    Localhost = cmkit:node_host_short(node()),
    lists:member(Localhost, Hosts).

state() ->
    state(length(current_nodes()), length(expected_nodes())).

state(Nodes, Hosts) when Nodes == Hosts ->
    green;

state(Nodes, Hosts) when Nodes =< Hosts/2 ->
    red;

state(_, _) ->
    yellow.

cursor(Env, Db) ->
    {ok, Txn} = elmdb:ro_txn_begin(Env),
    {ok, Cur} = elmdb:ro_txn_cursor_open(Txn, Db),
    Cur.

tail(Env, Db, Count) ->
    Cur = cursor(Env, Db),
    prev(Cur, Count, Count, [], 0).

prev(Cur, _, 0, Res, Hits) ->
    ok = elmdb:ro_txn_cursor_close(Cur),
    {ok, Res, Hits};

prev(Cur, Count, Count, Res, Hits) ->
    case elmdb:ro_txn_cursor_get(Cur, last) of
        not_found -> 
            ok = elmdb:ro_txn_cursor_close(Cur),
            {ok, Res, Hits+1};
        {ok, K, V} ->
            prev(Cur, Count, Count-1, [{ erlang:binary_to_term(K),
                                 erlang:binary_to_term(V)}|Res], Hits)
    end;

prev(Cur, Count, Rem, Res, Hits) ->
    case elmdb:ro_txn_cursor_get(Cur, prev) of
        not_found ->
            ok = elmdb:ro_txn_cursor_close(Cur),
            {ok, Res, Hits+1};
        {ok, K, V} ->
            prev(Cur, Count, Rem-1, [{erlang:binary_to_term(K), 
                              erlang:binary_to_term(V)}|Res], Hits+1)
    end.

it(Env, Db, K) ->
    Cur = cursor(Env, Db),
    Start = {set_range, erlang:term_to_binary(K)},
    case elmdb:ro_txn_cursor_get(Cur, Start) of
        not_found -> 
            {ok, [], 1};
        {ok, K2, V} -> 
            case erlang:binary_to_term(K2) of
                K -> 
                    next(Cur, K, 1, [{K, erlang:binary_to_term(V)}]);
                _ ->
                    {ok, [], 1}
            end
    end.


next(Cur, K, Iterations, Res) ->
    case elmdb:ro_txn_cursor_get(Cur, next) of 
        not_found -> 
            {ok, Res, Iterations};
        {ok, K2, V} ->
            case erlang:binary_to_term(K2) of
                K -> 
                    next(Cur, K, Iterations +1, [{K, erlang:binary_to_term(V)}|Res]);
                _ ->
                    {ok, Res, Iterations + 1}
            end
    end.


it(Env, Db, K, Count) ->
    Cur = cursor(Env, Db),
    next(Cur, K, K, Count, []).

next(Cur, _Since, _Count, 0, Res) ->
    ok = elmdb:ro_txn_cursor_close(Cur),
    Res;

next(Cur, Since, Count, Count, Res) ->
    Start = {set_range, erlang:term_to_binary(Since)},
    {ok, K, V} = elmdb:ro_txn_cursor_get(Cur, Start),
    next(Cur, Since, Count, Count-1, [{ erlang:binary_to_term(K), 
                                           erlang:binary_to_term(V)}|Res]);

next(Cur, Since, Count, Rem, Res) ->
    {ok, K, V} = elmdb:ro_txn_cursor_get(Cur, next),
    next(Cur, Since, Count, Rem-1, [{ erlang:binary_to_term(K), 
                                      erlang:binary_to_term(V)}|Res]).

ok(ok) -> true;
ok(_) -> false.

all_ok(Res) ->
    case lists:all(fun ok/1, Res) of
        true -> ok;
        false -> inconsistent
    end.
