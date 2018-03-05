-module(cmweb_ping).
-behaviour(cmweb).
-export([apps/0, spec/0, key/0, do/2]).

apps() -> all.

spec() ->
  {data, #{message => pong}}.

key() -> ping.

do(_, _) -> undefined.

