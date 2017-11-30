-module(cmweb_app).
-behaviour(application).
-export([start/2, stop/1]).
-export([init/2]).
-export([info/3]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2]).
-record(state, {app, debug, handlers, token, input, action, fsm, data}).

start(_StartType, _StartArgs) ->
  case cmweb_sup:start_link() of 
    {ok, Pid } ->
        Handlers = handlers(),
        Apps = cmkit:config(apps, cmweb), 
        [ listen(App, Port, Acceptors, Handlers)
          || {App, Port, Acceptors} <- Apps],
        {ok, Pid};      
    Other -> Other
  end.

stop(_State) ->
  ok.

listen(App, Port, Acceptors, Handlers) ->
    Dispatch = cowboy_router:compile([{'_', routes(App, Handlers)}]),
    {ok, _} = cowboy:start_clear(App, 
                                 [{port, Port}, {num_acceptors, Acceptors}],
                                 #{env => #{dispatch => Dispatch},
                                  stream_handlers => [cowboy_compress_h,
                                                     cowboy_stream_h]}),
    cmkit:log({cmweb, started, App, Port}).

routes(App, Handlers) ->
  Debug = cmkit:config(debug, App, false),
  State = #state{app=App, debug=Debug, handlers = Handlers},
  [
   {"/api/[...]", ?MODULE, State},
   {"/files/[...]", ?MODULE, State},
   {"/ws", ?MODULE, State},
   {"/", cowboy_static, {priv_file, App, "index.html"}},
   {"/[...]", cowboy_static, {priv_dir, App, "."}}
  ].

handlers() ->
    lists:foldl(fun(P, Handlers) ->
                        maps:put([erlang:atom_to_binary(P:key(), latin1)],
                                 P, Handlers)
                end, #{}, cmweb:all()).


init(Req, State) ->
  case cowboy_req:path(Req) of
    <<"/ws">> -> 
      init_ws(Req, State);
    <<"/files">> ->
      init_files(Req, State);
    _ ->
      init_http(Req, State)
  end.


init_http(Req, #state{handlers=Handlers}=State) ->
    PathInfo = cowboy_req:path_info(Req),
    case cmweb_util:module(PathInfo, Handlers) of
        undefined ->
            cmweb_util:not_found(no_route, Req, State);
        Module ->
            case Module:spec() of 
                {data, Data} ->
                    cmweb_util:ok(Data, Req, State);
                Params -> 
                    Body = #{},
                    case cmkit:parse(Params, Body) of
                        {errors, Errors} ->
                            cmweb_util:invalid(Errors, Req, State);
                        {ok, Input} -> 
                            %% TODO: fetch user from session
                            User = anonymous,
                            case Module:do(Input, User) of
                                {ok, Data} ->
                                    cmweb_util:ok(Data, Req, State);
                                {ok, Action, Data, _} ->
                                    cmweb_util:ok(Action, Data, Req, State);
                                {error, E} ->
                                    cmweb_util:err(E, Req, State)
                            end
                    end
            end

    end.


init_files(Req, #state{handlers=_Handlers}=State) ->
    Req1 = cmweb_util:cors(<<"POST, OPTIONS">>, <<"X-Session">>, Req),
    case cowboy_req:method(Req) of
        <<"OPTIONS">> ->
            cmweb_util:ok(#{}, Req1, State);
        <<"POST" >> ->
            case cmweb_util:file(Req1) of
                {ok, File, Req2} ->
                    cmweb_util:ok(File, Req2, State);
                {error, Error} ->
                    cmweb_util:invalid(Error, Req1, State)
            end;
        _ -> 
            cmweb_util:invalid(http_method, Req1, State)
    end.


info({data, Data}, Req, S) ->
  cmweb_util:ok(Data, Req, S);

info({not_found, R}, Req, S) ->
  cmweb_util:not_found(R, Req, S);

info({forbidden, R}, Req, S) ->
  cmweb_util:forbidden(R, Req, S);

info({error, R}, Req, S) ->
  cmweb_util:err(R, Req, S);

info({redirect, Loc, Cookies}, Req, S) ->
  cmweb_util:redirect(Loc, Cookies, Req, S).


init_ws(Req, #state{handlers=_Handlers}=State) ->
  Token = cmweb_util:cookie("cmtoken", Req),
  Input0 = #{<<"token">> => Token},
  State2 = State#state{input=Input0, token=Token, data=#{}},
  {cowboy_websocket, Req, State2}.

websocket_init(#state{app=_App, token=Token}=State) ->
    SessionId = case Token of
                    undefined -> cmkit:uuid();
                    _ -> Token
                end,
    cmkit:log({ws_started, {SessionId, self()}}),
    cmweb_util:ws_ok(#{session=>SessionId}, connect, State).


websocket_handle({text, Text}, #state{handlers=Handlers, input=Input0, token=_Token, data=Data}=State) ->
    case cmkit:jsond(Text) of
        {error, _} ->
            {stop, State};
        #{<<"action">> := Action}=Body0 ->
            cmweb_util:log(ws_in, Body0),
            Body = maps:merge(Body0, Input0),
            State2 = State#state{action=Action},
            case cmweb_util:module([Action], Handlers) of
                undefined ->
                    cmweb_util:ws_not_found(Action, State2);
                Module ->
                    case Module:spec() of
                        {data, D} ->
                            cmweb_util:ws_ok(D, Action, State2);
                        {data, Params, F} ->
                            case cmkit:parse(Params, Body) of
                                {errors, Errors} ->
                                    cmweb_util:ws_invalid(Errors, Action, State2);
                                {ok, Input} ->
                                    {ok, Data} = F(Input),
                                    cmweb_util:ws_ok(Data, Action, State2)
                            end;
                        Spec ->
                            case cmkit:parse([{text, <<"session">>}], Body) of
                                {errors, Errors }->
                                    cmweb_util:ws_invalid(Errors, Action, State2);
                                {ok, #{<<"session">> := _SessionId}} ->
                                    case cmkit:parse(Spec, Body) of
                                        {errors, Errors} ->
                                            cmweb_util:ws_invalid(Errors, Action, State2);
                                        {ok,  Input} ->
                                            case Module:do(Input, Data) of
                                                {noreply, Data2} ->
                                                    S3 = State2#state{data=Data2},
                                                    {ok, S3};
                                                {ok, Data2} ->
                                                    S3 = State2#state{data=Data2},
                                                    cmweb_util:ws_ok(#{}, Action, S3);
                                                {ok, Action2, Data2} ->
                                                    S3 = State2#state{data=Data2},
                                                    cmweb_util:ws_ok(#{}, Action2, S3);
                                                {ok, Action2, Reply, Data2} ->
                                                    S3 = State2#state{data=Data2},
                                                    cmweb_util:ws_ok(Reply, Action2, S3);
                                                {error, Reason, Data2} ->
                                                    S3 = State2#state{data=Data2},
                                                    cmweb_util:ws_error_with_reason(Reason, Action, S3);
                                                stop ->
                                                    {stop, State2}
                                            end
                                    end
                            end

                    end
            end
    end.

websocket_info(ack, #state{action=Action}=State) ->
    cmweb_util:ws_ack(Action, State);

websocket_info({error, R}, #state{action=Action}=State) ->
    cmweb_util:ws_error_with_reason(R, Action, State);

websocket_info({Action, Msg}, State) ->
    cmweb_util:ws_ok(Msg, Action, State);

websocket_info(Msg, #state{action=Action}=State) ->
    cmweb_util:ws_ok(Msg, Action, State).

