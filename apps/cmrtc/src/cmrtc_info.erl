-module(cmrtc_info).
-export([spec/0]).

spec() ->
  {data, cmperf:stats()}.
