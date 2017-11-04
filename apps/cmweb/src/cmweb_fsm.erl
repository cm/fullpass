-module(cmweb_fsm).
-behaviour(gen_fsm).
-export([start_link/5]).
-export([init/1, code_change/4, handle_event/3, handle_info/3,
         handle_sync_event/4, terminate/3]).
-export([authenticating/2]).
-record(data, {spec, module, input, token, conn}).

start_link(Spec, Module, Input, Token, Conn) ->
    gen_fsm:start_link(?MODULE, [Spec, Module, Input, Token, Conn], []).

init([Spec, Module, Input, Token, Conn]) ->
    Data = #data{spec=Spec, token=Token, module=Module, input=Input, conn=Conn},
    {ok, authenticating, Data, 0}.

authenticating(timeout, #data{spec={anonymous, _}, input=Input, module=Module, 
                              token=undefined, conn=Conn}=Data) ->
  Conn ! ack, 
  Module:anonymous([Conn|Input]),
  {next_state, waiting, Data};

authenticating(timeout, #data{spec={anonymous, _}, token=_, conn=Conn}=Data) ->
  Conn ! {forbidden, too_many_privileges},
  {stop, forbidden, Data};

authenticating(timeout, #data{spec={_Role, _}, token=undefined, conn=Conn}=Data) ->
  Conn ! {forbidden, insufficient_privileges},
  {stop, forbidden, Data};

authenticating(timeout, #data{spec={_Role, _}, token=_Token, conn=Conn}=Data) ->
    Conn ! authenticating,
    {next_state, authenticating, Data}.

handle_info(Msg, waiting, #data{conn=Conn}=Data) ->
    Conn ! Msg,
    {next_state, waiting, Data}.

handle_event(_Event, _StateName, StateData) ->
  {stop, badmsg, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
  {stop, badmsg, StateData}.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

terminate(_Reason, _SN, _SD) ->
  ok.
