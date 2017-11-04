-module(cmrtc_offer).
-export([spec/0]).
-export([anonymous/1]).

spec() ->
    {anonymous, [{text, <<"room">>}, {text, <<"sdp">>}]}.

anonymous([Conn, Room, Sdp]) ->
  cmcluster:dispatch({room, Room}, [offer, Sdp, Conn]).

