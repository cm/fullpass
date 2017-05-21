-module(fullpass_login_worker_sup).
-behaviour(supervisor).
-export([start_link/0, start_children/1]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {{simple_one_for_one, 1, 5}, [
                                     kit:child_spec(fpauth_worker, worker)
                                    ]}}.

start_child(Id) ->
  supervisor:start_child(?MODULE, [Id]).

start_children(Count) ->
  [start_child(X)|| X <- lists:seq(1, Count)].
