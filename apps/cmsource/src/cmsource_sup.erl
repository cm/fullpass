-module(cmsource_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  {ok, {{one_for_one, 1, 5}, [
    cmkit:child_spec(cmsource_cmd_server, worker),
    cmkit:child_spec(cmsource_query_server, worker),
    cmkit:child_spec(cmsource_event_server, worker),
    cmkit:child_spec(cmsource_alarm_server, worker)
  ]}}.
