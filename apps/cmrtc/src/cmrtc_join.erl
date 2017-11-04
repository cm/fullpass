-module(cmrtc_join).
-export([anonymous/1]).
-export([spec/0]).

spec() ->
  {anonymous, [{text, <<"room">>}]}.

anonymous([Conn, Room]) ->
  cmcluster:dispatch(room, [join, Room, Conn]).

