-module(cmrtc_complete).
-export([spec/0]).
-export([anonymous/1]).

spec() ->
    {anonymous, [{text, <<"room">>}]}.

anonymous([Conn, Room]) ->
    cmcluster:dispatch({room, Room}, [complete, Conn]).

