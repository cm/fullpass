-module(cmhttp).
-export([
  start/2, 
  init/2, info/3,
  websocket_init/1,  
  websocket_handle/2, 
  websocket_info/2
]).
-record(state, {app, debug, handler, req, action, spec, user, params}).


routes(App, Handler) -> 
  Debug = cmkit:config(debug, App, false),
  State = #state{app = App, debug=Debug, handler = Handler},
  [ 
    {"/api/[...]", ?MODULE, State},
    {"/ws", ?MODULE, State},
    {"/", cowboy_static, {priv_file, App, "index.html"}},
    {"/[...]", cowboy_static, {priv_dir, App, "."}}
  ].

start(App, Handler) -> 
  Port = cmkit:config(http_port, App),
  Dispatch = cowboy_router:compile([{'_', routes(App, Handler)}]),
  {ok, _} = cowboy:start_clear(my_http_listener, cmkit:config(http_acceptors, App),
                               [{port, Port}],
                               #{env => #{dispatch => Dispatch}}
                              ).

init(Req, State) ->
  State2 = State#state{req = Req},
  case cowboy_req:path(Req) of
    <<"/ws">> -> 
      init_ws(Req, State2);
    _ ->
      init_http(Req, State2)
  end.

init_http(Req, #state{handler = Handler}=State) ->
  PathInfo = cowboy_req:path_info(Req),
  Specs=Handler:do(PathInfo),
  case Specs of
    {data, Data} ->
      State2 = State#state{action=PathInfo},
      finish(Data, State2);
    {_Role, Act}=Spec ->
      State2 = State#state{action=PathInfo, spec=Spec},
      finish(user, [], Act, State2);
    {_Role, Params, Act}=Spec ->
      with_input(Params, Req, fun(Input) ->
                                  State2 = State#state{action=PathInfo, spec=Spec},
                                  finish(user, Input, Act, State2)
                              end);
    {_Role, Params, Act, _} = Spec ->
      with_input(Params, Req, fun(Input) ->
                                  State2 = State#state{action=PathInfo, spec=Spec},
                                  loop(user, Input, Act, State2)
                              end);
      
    _ -> not_found(Req)
  end.

with_input(Params, Req, Next) ->
  case input(Params, Req) of
    {errors, Errors} -> 
      invalid(Req, Errors);
    {ok, Input} ->
      Next(Input)
  end.

finish(User, Params, Act, #state{app=App, req=Req}=State) ->
  case Act(App, User, Params) of
    {error, E} -> 
      {ok, reply(500, E, [], Req), State};
    {data, B} -> 
      {ok, reply(200, B, [], Req), State};
    {redirect, Loc, Cookies} ->
      {ok, redirect(Loc, Cookies, Req), State}
  end.

finish(B, #state{req=Req}=State) ->
  {ok, reply(200, B, [], Req), State}.

loop(User, Params, Act, #state{app=App, req=Req}=State) ->
  Act(App, User, Params),
  State2 = State#state{user=User, params=Params},
  {cowboy_loop, Req, State2, hibernate}.

info(Msg, Req, #state{spec=Spec, app=App}=State) ->
  {_, _, _, React} = Spec,
  case React(App, user, Msg) of 
    {data, Body} -> 
      {stop, ok(Body, [], Req), State};
    {redirect, Loc, Cookies} ->
      {stop, redirect(Loc, Cookies, Req), State};
    continue ->
      {ok, Req, State}
  end.

init_ws(Req, #state{req=Req}=State) ->
  Input = read_session_cookie(#{}, Req),
  State2 = State#state{params=Input}, 
  {cowboy_websocket, Req, State2}.

websocket_init(State) ->
  {ok, State}.

websocket_handle({text, Text}, #state{handler=Handler, params=Input0}=State) ->
  case cmkit:jsond(Text) of
    {error, _} ->
      {stop, State};
    #{<<"action">> := Action} = Json ->
      debug(in, Json, State),
      Specs=Handler:do([Action]),
      State2=State#state{action=Action},
      case Specs of
        {data, Data} ->
          ws_ok(Data, State2);
        {_Role, Params, Act}=Spec ->
          Input1 = maps:merge(Json, Input0),
          ws_handle(Input1, Params, Act, Spec, State2);
        {_Role, Params, Act, _}=Spec ->
          Input1 = maps:merge(Json, Input0),
          ws_handle(Input1, Params, Act, Spec, State2);
        _ ->
          ws_error(#{ 
            <<"message">> => <<"not implemented">>
           }, State2)
      end;
    _ ->
      {stop, State}
  end.

websocket_info(Msg, #state{app=App, spec={_, _, _, React}}=State) ->
  case React(App, user, Msg) of
    continue -> 
      {ok, State};
    {data, Data} ->
      ws_ok(Data, State)
  end.

ws_handle(Input, Params, Act, Spec, #state{app=App}=State) ->
  State3=State#state{params=Input, spec=Spec},
  case cmkit:parse(Params, Input) of
    {errors, K} ->
      ws_invalid(K, State3);
    {ok, Input2} -> 
      case Act(App, user, Input2) of
        {data, Data} -> 
          ws_ok(Data, State3);
        _ ->
          {ok, State3}
      end
  end.

ws_ok(Data, State) ->
  ws_reply(Data, <<"ok">>, State).

ws_invalid(Data, State) ->
  ws_reply(Data, <<"invalid">>, State).

ws_error(Data, State) ->
  ws_reply(Data, <<"error">>, State).

ws_reply(Data, Status, #state{action=Action}=State) ->
  Reply = #{ <<"data">> => Data,
             <<"action">> => Action,
             <<"status">> => Status },
  debug(out, Reply, State),
  {reply, {text, jiffy:encode(Reply)}, State}.

debug(Prefix, Data, #state{debug=Debug}) ->
  case Debug of 
    true ->
      io:format("[~p] ~p~n", [Prefix, Data]);
    false ->
      ok
  end.



ok(Body, _Headers, Req) ->
  reply(200, Body, [], Req).

err(Reason, Req)->
  reply(500, Reason, [], Req).

%forbidden(Req) -> reply(403, Req).
not_found(Req) -> reply(404, Req).
invalid(Req, R) -> reply(400, #{reason => R}, [], Req).

reply(Status, Req) ->
  reply(Status, [], [], Req).

reply(Status, Body, _Headers, Req) ->
  cowboy_req:reply(Status, #{
                     <<"content-type">> => <<"application/json">>
                    }, jiffy:encode(Body), Req).


redirect(Loc, Cookies, Req) ->
  Req2 = maps:fold(fun(K, V, R) ->
    cowboy_req:set_resp_cookie(K, V, R, #{path => <<"/">>})
  end, Req, Cookies),
  cowboy_req:reply(303, #{
    <<"location">> => Loc  
  }, Req2).

input([], _Req) -> [];

input(Spec, Req) ->
  Input = parse_qs(Req),
  case cowboy_req:has_body(Req) of
    false -> 
      Input2 = read_session_cookie(Input, Req),
      cmkit:parse(Spec, Input2);
    true ->
      {ok, Body, Req2} = cowboy_req:read_body(Req),
      case cowboy_req:header(<<"content-type">>, Req2) of
        <<"application/json">> ->
          case cmkit:jsond(Body) of
            {error, Reason} -> 
              err(Reason, Req);
            Json ->
              Input2 = maps:merge(Input, Json),
              Input3 = read_session_cookie(Input2, Req),
              cmkit:parse(Spec, Input3)
          end;
        _ -> invalid(Req2, content_type)
      end
  end.   

read_session_cookie(Input, Req) ->
  Cookies = cowboy_req:parse_cookies(Req),
  case lists:keyfind(<<"token">>, 1, Cookies) of
    {_, T} -> maps:put(<<"token">>, T, Input);
    false -> Input
  end.
  

parse_qs(Req) ->
  lists:foldl(fun({K, V}, Map) ->
    maps:put(K, V, Map) 
  end, #{}, cowboy_req:parse_qs(Req)).

