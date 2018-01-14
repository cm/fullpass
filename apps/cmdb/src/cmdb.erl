-module(cmdb).
-export([behaviour_info/1, all_tables/0, table_for/1]).
-export([started/0, table_info/3, tables_info/0]).
-export([table_copies_to_media/1, media_to_table_copies/1, type_to_storage/1, storage_to_type/1]).
-export([add/3, add/1, drop/1, drop/2, create_schema/1, drop_schema/1, exists/1]).
-export([start/0, stop/0, info/0, subscribe/0, unsubscribe/0, event_for/1]).
-export([c/1, c/3, clear/0, await/1, k/1, b/1, u/3, u/4, i/3, i/4, d/3, d/4, r/2, r/3, s/2, s/3, m/3, m/4, j/4, j/6, j/7, j/8, w/1, uw/1, f/5, l/1, l/4, l/5, l/6, l/7]).
-record(triplet, {s, p, o}).

behaviour_info(callbacks) ->
  [{cmdb_tables, 0}].

all_tables() ->
    lists:flatten([M:cmdb_tables() || M <-erlang:loaded(), cmkit:implements(M, behaviour_info(callbacks))]).

table_for(Name) when is_binary(Name) ->
    Match = lists:filter(fun({N, _, _}) -> 
                         cmkit:to_bin(N) =:= Name
                end, all_tables()),
    case Match of 
        [{_, _, _}=Tab] ->
            {ok, Tab};
        [] ->
            {error, no_such_table};
        _ ->
            {error, duplicate_table}
    end.

start() ->
    mnesia:start().

stop() -> 
    mnesia:stop(),
    ok.

started() -> 
    case [T || {A, _, _} = T <- application:which_applications(), A =:= mnesia] of
        [] -> false;
        [{mnesia, _, _}] -> true
    end.


info() ->
    mnesia:info().

subscribe() -> 
    subscribe(system).

subscribe(Cat) ->
    mnesia:subscribe(Cat).

unsubscribe() ->
    unsubscribe(system).

unsubscribe(Cat) ->
    mnesia:unsubscribe(Cat).

event_for({T, Args}) ->
    {T, Args};

event_for({inconsistent_database, T, N}) ->
    {T, N};

event_for({T, F, _Args}) ->
    {T, F};

event_for({T, F, _Args, _BinaryCore}) ->
    {T, F}.

table_info(Tab, _Type, Copies) -> 
    #{ info => #{ id => encode_cookie(Tab),
                  name => Tab,
                  type => mnesia:table_info(Tab, type),
                  media => table_copies_to_media(Copies)
                },
       copies => #{ mem => lists:map(fun cmkit:node_host_short/1, mnesia:table_info(Tab, ram_copies)),
                    disc => lists:map(fun cmkit:node_host_short/1, mnesia:table_info(Tab, disc_only_copies)),
                    both =>  lists:map(fun cmkit:node_host_short/1, mnesia:table_info(Tab, disc_copies))},
       size => #{ count => mnesia:table_info(Tab, size),
                  words => mnesia:table_info(Tab, memory) } 
     }.

table_copies_to_media(disc_copies) -> both;
table_copies_to_media(disc_only_copies) -> disc;
table_copies_to_media(ram_copies) -> memory.

media_to_table_copies(both) -> disc_copies;
media_to_table_copies(<<"both">>) -> disc_copies;
media_to_table_copies(disc) -> disc_only__copies;
media_to_table_copies(<<"disc">>) -> disc_only_copies;
media_to_table_copies(memory) -> ram_copies;
media_to_table_copies(<<"memory">>) -> ram_copies.

storage_to_type(set) -> set;
storage_to_type(<<"set">>) -> set;
storage_to_type(bag) -> bag;
storage_to_type(<<"bag">>) -> bag;
storage_to_type(ordered_set) -> ordered_set;
storage_to_type(<<"ordered_set">>) -> ordered_set.

type_to_storage(T) -> cmkit:to_bin(T).

tables_info() ->
    case started() of 
        true -> 
            Tables = [ table_info(Tab, Type, Copies) || {Tab, Type, Copies} <- all_tables(), exists(Tab) ],
            [table_info(schema, set, disc_copies) | Tables];
        false -> 
            []
    end.

encode_cookie(T) ->
    C = mnesia:table_info(T, cookie),
    base64:encode(erlang:binary_to_list(erlang:md5(erlang:term_to_binary(C)))).

clear() ->
    [clear(Tab) || Tab <- all_tables()].

drop_schema(Node) ->
    cmkit:log({cmdb, deleting_schema, Node}),
    mnesia:stop(),
    R = mnesia:delete_schema([Node]),
    mnesia:start(),
    R.

create_schema(Node) ->
    cmkit:log({cmdb, creating_schema, Node}),
    mnesia:stop(),
    R = mnesia:create_schema([Node]),
    mnesia:start(),
    R.

drop(Tab, Node) ->
    case mnesia:del_table_copy(Tab, Node) of 
        {aborted, {no_exists, _}} ->
            {error, not_found};
        {aborted, {badarg, Tab, unknown}} ->
            {error, not_found};
        {atomic, ok} -> 
            ok
    end.

drop(Tab) ->
    mnesia:delete_table(Tab).

has_copies(Tab, Media) ->
    length(mnesia:table_info(Tab, Media)) > 0.

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

c({Tab, Storage, Media}) ->
    c({Tab, Storage, Media, [node()]});

c({Tab, Storage, Media, Nodes}) ->
    case has_copies(schema, disc_copies) of 
        true -> 
            case exists(Tab) of
                false -> 
                    case mnesia:create_table(Tab,
                                             [{attributes, record_info(fields, triplet)},
                                              {record_name, triplet},
                                              {type, storage_to_type(Storage)},
                                              {media_to_table_copies(Media), Nodes}]) of 
                        {atomic, ok} -> 
                            ok;
                        {aborted, Reason} -> 
                            {error, Reason}
                    end;
                true ->
                    {error, exists}
            end;
        false -> 
            {error, schema_not_initialized}
    end.

c(Tab, Storage, Media) ->
    case has_copies(schema, disc_copies) of
        true -> 
            Info = [{attributes, record_info(fields, triplet)},
                    {record_name, triplet},
                    {type, storage_to_type(Storage)}],
            Info2 = add_copies_info(Info, Media),
            case exists(Tab) of
                false -> 
                    case mnesia:create_table(Tab, Info2) of
                        {atomic, ok} -> 
                            ok;
                        {aborted, E} -> 
                            {error, E}
                    end;
                true ->
                    {error, exists}
            end;
        false -> 
            {error, schema_not_initialized}
    end.


add_copies_info(Info, []) -> 
    Info;

add_copies_info(Info, [{disc, []}|Rem]) ->
    add_copies_info(Info, Rem);

add_copies_info(Info, [{disc, Nodes}|Rem]) ->
    add_copies_info([{disc_only_copies, Nodes}|Info], Rem);

add_copies_info(Info, [{memory, []}|Rem]) ->
    add_copies_info(Info, Rem);

add_copies_info(Info, [{memory, Nodes}|Rem]) ->
    add_copies_info([{ram_copies, Nodes}|Info], Rem);

add_copies_info(Info, [{both, []}|Rem]) ->
    add_copies_info(Info, Rem);

add_copies_info(Info, [{both, Nodes}|Rem]) ->
    add_copies_info([{disc_copies, Nodes}|Info], Rem).

exists(TableName) ->
   Tables = mnesia:system_info(tables),
   lists:member(TableName,Tables).

add(Node, Tab, Type) ->
    case mnesia:add_table_copy(Tab, Node, Type) of
        {atomic, ok} -> ok;
        {aborted, R} -> {error, R}
    end.

add(Node) ->
    case mnesia:change_config(extra_db_nodes, [Node]) of 
        {ok, _} -> 
            case mnesia:change_table_copy_type(schema, Node, disc_copies) of
                {atomic, ok} -> ok;
                {aborted, R} -> {error, R}
            end;
        {error, R} -> 
            cmkit:log({cmdb, error, change_config, extra_db_nodes, Node, R}),
            {error, mnesia_error}
    end.

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

l(Tab) -> 
    case mnesia:activity(sync_dirty,
        fun() ->
             mnesia:foldl(
                fun(#triplet{o=V}, Acc) ->
              [V|Acc]
          end,
          [],
          Tab)
  end) of 
        {aborted, R} -> 
            {error, R};
        V -> 
            {ok, V}
    end.


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
