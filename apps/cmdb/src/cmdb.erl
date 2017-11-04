-module(cmdb).
-export([behaviour_info/1, all_tables/0]).
-export([start/0, info/0, c/1, clear/0, await/1, k/1, b/1, u/3, u/4, i/3, i/4, d/3, d/4, r/2, r/3, s/2, s/3, m/3, m/4, j/4, j/6, j/7, j/8, w/1, uw/1, f/5, l/4, l/5, l/6, l/7]).
-record(triplet, {s, p, o}).

behaviour_info(callbacks) ->
  [{cmdb_tables, 0}].

all_tables() ->
    lists:flatten([M:cmdb_tables() || M <-erlang:loaded(), cmkit:implements(M, behaviour_info(callbacks))]).

start() ->
    mnesia:stop(),
    mnesia:create_schema([node()]),
    mnesia:start().


info() ->
    mnesia:info().

clear() ->
    [clear(Tab) || Tab <- all_tables()].

clear({T, _, _}) ->
    case mnesia:clear_table(T) of
        {atomic, ok} ->
            cmkit:log({cmdb, clear_table, ok, T}),
            ok;
        {aborted, E} ->
            cmkit:log({cmdb, clear_table, aborted, T, E}),
            {error, E}
    end.

await(Tabs) -> 
    mnesia:wait_for_tables(Tabs, 3000).


c({Tab, Type, Storage}) ->
    case exists(Tab) of
        false -> 
            mnesia:create_table(Tab,
                                [{attributes, record_info(fields, triplet)},
                                 {record_name, triplet},
                                 {type, Type},
                                 {Storage, [node()]}]);
        true ->
            exists
    end.

exists(TableName) ->
   Tables = mnesia:system_info(tables),
   lists:member(TableName,Tables).

t(F) ->
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        {atomic, conflict} -> conflict;
        {atomic, R} -> {ok, R};
        {aborted, Reason} -> {aborted, Reason};
        E -> {error, E}
    end.

k(Tab) -> 
    t(fun() -> mnesia:all_keys(Tab) end).

e({u, T, S, P, O}) ->
    mnesia:write(T, #triplet{s=S, p=P, o=O}, write);

e({u, T, S, O}) ->
    e({u, T, S, is, O});

e({a, T, S, P, O}) ->
    e({u, T, S, P, O});

e({a, T, S, O}) ->
    e({a, T, S, is, O});
    
e({i, T, S, P, O}) ->
    case mnesia:read(T, S, write) of
        [] ->
            e({u, T, S, P, O});
        [_] ->
            conflict;
        Other  -> 
            Other
    end;

e({i, T, S, O}) ->
    e({i, T, S, is, O});

e({d, T, S, P, O}) ->
    mnesia:delete_object(T, #triplet{s=S, p=P, o=O}, write);

e([]) -> ok;

e([H|T]) ->
    case e(H) of
        ok -> e(T);
        Other ->
            mnesia:abort("batch error"),
            Other
    end.

b([]) -> ok;

b([_|_]=All) ->
    t(fun() ->
        e(All)
    end).

u(T, S, O) ->
    u(T, S, is, O).

u(T, S, P, O) ->
    t(fun() ->
        e({u, T, S, P, O})
    end).

i(T, S, P, O) ->
    t(fun() ->
        e({i, T, S, P, O})
    end).

i(T, S, O) ->
    i(T, S, is, O).

d(T, S, P, O) ->
    t(fun() ->
        e({d, T, S, P, O})
    end).

d(T, S, O) ->
    d(T, S, is, O).

r(T, S) ->
    case mnesia:dirty_read(T, S) of
        [] -> not_found;
        [#triplet{s=S, o=O}] -> {ok, O};
        [_|_]=L -> {ok, object(lists:last(L))};
        E -> {error, E}
    end.

r(T, S, P) ->
    case s(T, S, P) of
        {ok, [V]} -> {ok, V};
        {ok, []} -> not_found;
        {ok, [_|_]=L} -> {ok, object(lists:last(L))};
        {error, E} -> {error, E}
    end.

object(#triplet{o=O}) ->
    O;

object(M) when is_map(M) -> 
    M;

object(Other) -> 
    {unknown, Other}.


by_pred([], R) -> R;
by_pred([#triplet{p=P, o=O}|T], R) ->
    case maps:is_key(P, R) of
        true ->
            Objects = maps:get(P, R),
            by_pred(T, maps:put(P, [O|Objects], R));
        false ->
            by_pred(T, maps:put(P, [O], R))
    end.

by_pred(L) -> by_pred(L, #{}).

s(T, S) ->
    case mnesia:dirty_read(T, S) of
        [] -> not_found;
        [_|_]=R -> {ok, by_pred(R)};
        E -> {error, E}
    end.

s(T, S, P) ->
    case s(T, S) of
        {ok, Preds} ->
            case maps:is_key(P, Preds) of 
                false -> 
                    {ok, []};
                true ->
                    {ok, maps:get(P, Preds)}
            end;
        Other -> Other
    end.

deref([], _, R) -> R;
deref([Id|T], M, R) when is_binary(Id) or is_tuple(Id) -> 
    case r(M, Id, is) of
        {ok, V} -> 
            deref(T, M, [V|R]);
        _ ->
            deref(T, M, R)
    end;

deref([#triplet{o=Id}|T], M, R) ->
    case r(M, Id, is) of
        {ok, V} -> 
            deref(T, M, [V|R]);
        _ ->
            deref(T, M, R)
    end.


m(T, S, O) ->
    m(T, S, is, O).

m(T, S, P, O) ->
    case t(fun() ->
        mnesia:match_object(T, #triplet{s=S, p=P, o=O}, read)
    end) of
        {ok, R} ->
            {ok, length(R)};
        Other -> 
            Other
    end.


j(T, S, P, M) ->
    case s(T, S, P) of 
        {ok, Ids} -> 
            Distinct = cmkit:distinct(Ids),
            {ok, deref(Distinct, M, [])};
        Other -> 
            Other
    end.

j(T, S, P, M, From, Callback) ->
    case s(T, S, P) of 
        {ok, Ids} -> 
            Distinct = cmkit:distinct(Ids),
            [ From ! {Callback, V} || V <- deref(Distinct, M, []) ],
            ok;
        Other -> 
            Other
    end.

j(T, S, P, M, Fun, From, Callback) ->
    case s(T, S, P) of 
        {ok, Ids} -> 
            [ From ! {Callback, V} || V <- deref(Ids, M, []), Fun(V) == true ],
            ok;
        Other -> 
            Other
    end.

j(T, S, P, M, Filter, Map, From, Callback) ->
    case s(T, S, P) of 
        {ok, Ids} -> 
            [ From ! {Callback, Map(V)} || V <- deref(Ids, M, []), Filter(V) == true ],
            ok;
        Other -> 
            Other
    end.


w(Tab) ->
    mnesia:subscribe({table, Tab, simple}).

uw(Tab) ->
    mnesia:unsubscribe({table, Tab, simple}).

l(Tab, K, From, Callback) -> 
    l(Tab, K, is, undefined, From, Callback).

l(Tab, K, M, From, Callback) -> 
    l(Tab, K, is, M, From, Callback).

l(Tab, K, P, M, From, Callback) -> 
    case t(fun() -> 
        mnesia:select(Tab, [{#triplet{s=K, p=P, o='_'}, [], ['$_']}])
    end) of 
        {ok, Results} -> 
            case M of 
                undefined -> 
                    [ From ! {Callback, V} || V <- Results];
                _ ->
                    [ From ! {Callback, V} || V <- deref(Results, M, [])]
            end,
            ok;
        Other -> 
            Other
    end.

l(Tab, K, P, Filter, M, From, Callback) -> 
    case t(fun() -> 
        mnesia:select(Tab, [{#triplet{s=K, p=P, o='_'}, [], ['$_']}])
    end) of 
        {ok, Results} -> 
            case M of 
                undefined -> 
                    [ From ! {Callback, V} || V <- Results];
                _ ->
                    [ From ! {Callback, V} || V <- deref(Results, M, []), Filter(V) == true]
            end,
            ok;
        Other -> 
            Other
    end.


f(Tab, Keyword, Fields, From, Callback) ->
    t(fun() ->
              F = fun(#triplet{o=O}, Acc) ->
                          case is_map(O) and cmkit:search_map(Keyword, Fields, O) of
                              true -> 
                                  From ! {Callback, O},
                                  Acc + 1;
                              false -> 
                                  Acc
                          end
                    end,
              mnesia:foldl(F, 0, Tab)
      end).
