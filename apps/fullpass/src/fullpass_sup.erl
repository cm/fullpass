-module(fullpass_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  {ok, {{one_for_one, 1, 5}, [
    cmkit:child_spec(cmaggregate, [fullpass_login], supervisor)
  ]}}.
