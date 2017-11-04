-module(cmpresence_sup).
-behaviour(supervisor).
-export([start_link/0, update/3]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

update(Uid, Gid, App) ->
    case cmpresence_worker:whereis(Uid, Gid, App) of
        undefined ->
            supervisor:start_child(?MODULE, [Uid, Gid, App]);
        Pid -> 
            cmpresence_worker:update(Pid, Uid, Gid, App)
    end.

init([]) ->
    {ok, { {simple_one_for_one, 0, 1}, [ cmkit:child_spec(cmpresence_worker, 
                                                          cmpresence_worker,
                                                          [],
                                                          transient,
                                                          worker) ]}}.
