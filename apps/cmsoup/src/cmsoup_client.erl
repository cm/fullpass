-module(cmsoup_client).
-behaviour(cmwsc).
-export([start_link/0]).
-export([in/1, out/1]).

start_link() ->
    cmwsc:start_link(?MODULE, cmsoup_client_ws, config()).

config() ->
    { cmkit:config(server, cmsoup), 
      cmkit:config(port, cmsoup), 
      cmkit:config(path, cmsoup)
    }.

in(#{<<"action">> := <<"join">>,
     <<"room">> := Room,
     <<"participant">> := Uid,
     <<"offer">> := Offer}) ->
    case cmdb:s(connections, Uid, has) of
        {ok, Conns} ->
            [ C ! {join, #{ sdp => Offer,
                            status => offer,
                            group => Room}} || C <- Conns ];
        _ ->
            cmkit:log({join, no_connections, Uid})
    end;

in(Msg) ->
    cmkit:log({cmsoup_client, in, Msg}).

out(Msg) ->
    cmkit:log({cmsoup_client, out, Msg}),
    cmwsc:out(cmsoup_client_ws, Msg).
