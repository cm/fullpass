-module(cmdb_reset).
-behaviour(cmweb).
-export([spec/0, key/0, do/2]).

key() -> reset.

spec() -> [].

do(#{}, State) ->
    cmdb:clear(),
    {ok, State}.
