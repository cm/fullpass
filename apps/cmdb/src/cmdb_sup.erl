-module(cmdb_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Dbs = lists:map(fun(Props) ->
                        maps:from_list(Props)
                        %#{ replicas := Hosts } = Db = maps:from_list(Props),
                        %{ok, Nodes }  = cmkit:hosts_to_nodes(Hosts),
                        %Db#{ replicas => Nodes }
                    end, cmkit:config(dbs, cmdb)),
    

    Replicas = [ cmkit:child_spec(Id, cmdb_replica, [Db], worker)
                 || #{ name := Id}=Db <- Dbs, cmdb_util:is_local(Db) ],
        
    Routers = [ cmkit:child_spec(R, R, [Dbs], worker) 
                || R <- [cmdb_read, cmdb_write]],
    
    {ok, {{one_for_one, 0, 1}, Replicas ++ Routers }}.
