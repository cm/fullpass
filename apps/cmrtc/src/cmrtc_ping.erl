-module(cmrtc_ping).
-export([spec/0, key/0, do/2]).

spec() ->
  {data, #{message => pong}}.

key() -> ping.

do(_, _) -> undefined.

