-module(cmrtc_ice).
-export([spec/0]).
-export([anonymous/1]).

spec() ->
    {anonymous, [{text, <<"room">>}, {text, <<"ice">>}]}.

anonymous([Conn, Room, Ice]) ->
  cmcluster:dispatch({room, Room}, [ice, Ice, Conn]).

