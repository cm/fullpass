-module(cmdb_server).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).
-record(data, {beacon, role, monitor, slaves}).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
    %{ok, B} = rbeacon:new(9999, [active, noecho]),
    %rbeacon:set_interval(B, 300),
    %rbeacon:subscribe(B, <<>>),
    %rbeacon:publish(B, erlang:term_to_binary({cmdb, node()})),
    {ok, #data{}, 0}.

%handle_info({rbeacon, _, Msg, _}, #data{beacon=B}=State) ->
%    {cmdb, P} = erlang:binary_to_term(Msg),
%    case net_adm:ping(P) of
%        pong -> 
%            cmkit:log({cmdb, ping, P, pong}),
%            rbeacon:silence(B),
%            {noreply, State};
%        pang ->
%            cmkit:log({cmdb, ping, P, pang}),
%            {noreply, State}
%    end;

handle_info(timeout, #data{}=State) ->
    mnesia:stop(),
    case mnesia:create_schema([node()]) of 
        ok ->
            cmkit:log({cmdb, master, schema_created});
        {error, E} ->
            cmkit:log({cmdb, master, schema_not_created, E})
    end,
    mnesia:start(),
    create_tables(),
    init_tables(),
    {noreply, State#data{role=master}}.

%handle_info(timeout, #data{beacon=B}=State) ->
%    rbeacon:silence(B),
%    case attempt_master() of
%        yes -> 
%            mnesia:stop(),
%            case mnesia:create_schema([node()]) of 
%                ok ->
%                    cmkit:log({cmdb, master, schema_created});
%                {error, E} ->
%                    cmkit:log({cmdb, master, schema_not_created, E})
%            end,
%            mnesia:start(),
%            create_tables(),
%            init_tables(),
%            {noreply, State#data{role=master}};
%        no ->
%            cmkit:log({cmdb, error, no_master}),
%            {stop, no_master, State};
%        Leader -> 
%            Ref = monitor_master(Leader),
%            {noreply, State#data{monitor=Ref}}
%    end;



%%handle_info({rbeacon, B, closed}, State) ->
%%    cmkit:log({cmdb, beacon, B, closed}),
%%    {noreply, State};
%%
%%handle_info({cmdb_slave, Pid, Name, Cookie, RemoteTables}, #data{role=master, slaves=Slaves}=State) ->
%%    case lists:member(Name, mnesia:system_info(running_db_nodes)) of
%%        false ->
%%            case Cookie =:= mnesia:table_info(schema, cookie) of 
%%                true ->
%%                    mnesia:change_config(extra_db_nodes, [Name]),
%%                    merge_tables(Name, RemoteTables),
%%                    cmkit:log({cmdb, new_slave, Name});
%%                false ->
%%                    case gen_server:call(Pid, reset) of 
%%                        ok -> 
%%                            mnesia:change_config(extra_db_nodes, [Name]),
%%                            mnesia:change_table_copy_type(schema, Name, disc_copies),
%%                            merge_tables(Name, RemoteTables),
%%                            cmkit:log({cmdb, reset_slave, Name});
%%                        {error, E} -> 
%%                            cmkit:log({cmdb, cannot_reset_slave, E, Name})
%%                    end
%%            end;
%%        true ->
%%            merge_tables(Name, RemoteTables),
%%            cmkit:log({cmdb, existing_slave, Name})
%%    end,
%%    {noreply, State#data{slaves=[Pid|Slaves]}};
%%
%%handle_info({'DOWN', Ref, process, _, Reason}, #data{monitor=Ref}=State) ->
%%    cmkit:log({cmdb, master_down, Reason}),
%%    case attempt_master() of
%%        yes -> 
%%             {noreply, State#data{role=master}};
%%        no -> 
%%            cmkit:log({cmdb, error, no_master}),
%%            {stop, no_master, State};
%%        Other ->
%%            Ref2 = monitor_master(Other),
%%            {noreply, State#data{monitor=Ref2}}
%%    end.
%%
%%attempt_master() -> 
%%    case global:whereis_name(cmdb_master) of
%%        undefined ->
%%            case global:register_name(cmdb_master, self()) of
%%                yes -> 
%%                    cmkit:log({cmdb, master}),
%%                    yes;
%%                no ->
%%                    case global:whereis_name(cmdb_master) of
%%                        undefined ->
%%                            no;
%%                        Other -> Other
%%                    end
%%            end;
%%         Other -> Other
%%    end.
%%
%%monitor_master(Leader) ->
%%    Ref = erlang:monitor(process, Leader),
%%    cmkit:log({cmdb, slave, Leader, Ref}),
%%    global:send(cmdb_master, {cmdb_slave, self(), node(), 
%%                              mnesia:table_info(schema, cookie), local_tables()}),
%%    Ref.

handle_call(reset, Master, State) ->
    mnesia:stop(),
    cmkit:log({cmdb, reset, node(), Master}),
    R = mnesia:delete_schema([node()]),
    mnesia:start(),
    {reply, R, State}.
        
local_tables() ->
    [{T, mnesia:table_info(T, type), mnesia:table_info(T, storage_type)}
     || T <- mnesia:system_info(tables) -- [schema] ].


%%merge_tables(Node, RemoteTables) ->
%%    LocalTables = local_tables(),
%%    [merge_table(node(), T, LocalTables) || T <- RemoteTables],
%%    [merge_table(Node, T, LocalTables) || T <- LocalTables].
%%
%%
%%merge_table(Node, {Tab, _, Storage}=T, LocalTables) ->
%%    case is_table_member(Node, T, LocalTables) of
%%        true ->
%%            add_fragments(Tab, nodes());
%%            %cmkit:log({cmdb, exists, Node, Tab, Storage});
%%        false ->
%%            case mnesia:add_table_copy(Tab, Node, Storage) of 
%%                {atomic, ok} ->
%%                    add_fragments(Tab, [Node]);
%%                    %cmkit:log({cmdb, add_table_copy_ok, Node, Tab, Storage});
%%                {aborted, R} ->
%%                    cmkit:log({cmdb, add_table_copy_aborted, Node, Tab, Storage, R})
%%            end
%%    end.



add_fragments(Tab, Nodes) ->
    case mnesia:table_info(Tab, frag_properties) of 
        [] -> 
            mnesia:change_table_frag(Tab, {activate, {nodepool, Nodes}});
            %cmkit:log({cmdb, activate_table_frag_ok, Tab, Nodes});
        _ ->
            mnesia:change_table_frag(Tab, {add_frag, Nodes})
            %case mnesia:change_table_frag(Tab, {add_frag, Nodes}) of 
            %    {atomic, ok} -> 
            %        cmkit:log({cmdb, add_table_frag_ok, Tab, Nodes});
            %    {aborted, E} ->
            %        cmkit:log({cmdb, add_table_frag_error, Tab, Nodes, E})
            %end
    end.


%%is_table_member(Node, {Tab, _, Storage}, Tables) ->
%%    case table_exists(Tab, Tables) of
%%        true -> 
%%            case Storage of 
%%                unknown -> false;
%%                _ -> 
%%                    Members = mnesia:table_info(Tab, Storage),
%%                    lists:member(Node, Members)
%%            end;
%%        false -> false
%%    end.
%%
create_tables() ->
    [create_table(T, local_tables()) || T <- cmdb:all_tables()].

create_table({Tab, _, _}=T, Local) -> 
    case table_exists(Tab, Local) of
        false ->
            case cmdb:c(T) of 
                {atomic, ok} ->
                    cmkit:log({cmdb, table, created, Tab, mnesia:table_info(Tab, type)}),
                    add_fragments(Tab, [node()]);
                    %cmkit:log({cmdb, table_create_ok, Tab});
                {aborted, E} -> 
                    cmkit:log({cmdb, cmdb, table_create_error, Tab, E})
            end;

        true ->
            cmkit:log({cmdb, table, exists, Tab, mnesia:table_info(Tab, type)}),
            add_fragments(Tab, [node()])
            %cmkit:log({cmdb, table_exists, Tab})
    end.

table_exists(_, []) -> false;
table_exists(T, [{T, _, _}]) -> true;
table_exists(T, [{T, _, _}|_]) -> true;
table_exists(T, [_|Rem]) -> table_exists(T, Rem).

init_tables() ->
    Entries = [M:cmdb_init() || M <-erlang:loaded(), cmkit:implements(M, [{cmdb_init, 0}])],
    cmkit:log({cmdb, init, length(Entries)}),
    cmdb:b(Entries).
    
handle_cast(_, State) ->
  {noreply, State}.

terminate(_Reason, #data{beacon=B}) ->
    rbeacon:close(B).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
