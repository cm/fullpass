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
                    end, cmkit:config(dbs, cmdb)),
    
    Routers = [ db_spec(Db) || Db <- Dbs ],
    {ok, {{one_for_one, 0, 1}, Routers }}.

db_spec(#{ name := Name}=Db) ->
    cmkit:child_spec(Name, cmdb_client, [Db], worker).

