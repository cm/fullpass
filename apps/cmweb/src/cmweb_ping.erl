-module(cmweb_ping).
-behaviour(cmweb).
-export([spec/0, key/0, do/2]).

spec() ->
  {data, #{message => pong}}.

key() -> ping.

do(_, _) -> undefined.

