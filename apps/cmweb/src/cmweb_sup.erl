-module(cmweb_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([start_child/4]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Spec, Module, Input, Token) ->
  supervisor:start_child(?MODULE, [Spec, Module, Input, Token, self()]).

init([]) ->
  {ok, {{simple_one_for_one, 1, 1}, [
                                     child_spec(cmweb_fsm, [])
                                    ]}}.

child_spec(M, Args) ->
  {M, {M, start_link, Args}, temporary, brutal_kill, worker, [M]}.
