-module(cmpresence_worker).
-behaviour(gen_statem).
-export([start_link/3, update/1, whereis/3]).
-export([init/1, callback_mode/0, terminate/3]).
-export([away/3, online/3]).
-record(data, {user, group, app}).
-define(AWAY_TIMEOUT, 10000).
-define(OFFLINE_TIMEOUT, 10000).



callback_mode() ->
    state_functions.

start_link(Uid, Gid, App) ->
    gen_statem:start_link({global, {presence, Uid, Gid, App}}, 
                          ?MODULE, [Uid, Gid, App], []).

whereis(Uid, Gid, App) -> 
    global:whereis_name({presence, Uid, Gid, App}).

update(Pid) ->
    gen_statem:cast(Pid, update).

init([Uid, Gid, App]) ->
    cmkit:log({presence, online, Uid, Gid, App}),
    broadcast(Uid, Gid, App, online),
    {ok, online, #data{user=Uid, group=Gid, app=App},
        [{{timeout, away}, ?AWAY_TIMEOUT, online}]}.

online({timeout, away}, _, #data{user=Uid, group=Gid, app=App}=Data) ->
    broadcast(Uid, Gid, App, away),
    cmkit:log({presence, away}),
    {next_state, away, Data, 
        [{{timeout, terminating}, ?OFFLINE_TIMEOUT, terminating}]}.

away(cast, update, Data) ->
    {next_state, online, Data, 
        [{{timeout, away}, ?AWAY_TIMEOUT, online}] };

away({timeout, terminating}, _, #data{user=Uid, group=Gid, app=App}) ->
    broadcast(Uid, Gid, App, offline),
    cmkit:log({presence, offline}),
    {stop, normal}.

terminate(_Reason, _, _Data) ->
    ok.


broadcast(Uid, Gid, App, Status) ->
    P = #{ user => Uid, group => Gid, application => App, status => Status},
    case cmdb:u(presence, {Uid, Gid, App}, is, Status) of
        ok ->
            [ notify(Mid, P) || Mid <- members(Gid) ],
            ok;
        {error, E} ->
            {error, E}
    end.


members(Gid) -> 
    case cmdb:s(groups, Gid, has_member) of
        {ok, Uids} -> 
            Uids;
        Other ->
            cmkit:log({presence, members, Gid, unexpected, Other}),
            []
    end.

notify(Uid, P) ->
    case cmdb:s(connections, Uid, has) of
        {ok, Conns1} ->
            [ C ! {presence, P} || C <- Conns1 ],
            ok;
        Other -> 
            cmkit:log({presence, notify, Uid, unexpected, Other}),
            ok
    end.
