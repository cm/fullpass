-module(cmweb).
-export([start/4]).
-export([init/2]).
-record(state, {debug, handlers}).

start(App, Port, Acceptors, Handlers) ->
  Dispatch = cowboy_router:compile([{'_', routes(App, Handlers)}]),
  {ok, _} = cowboy:start_clear(my_http_listener, 
                               Acceptors, [{port, Port}],
                               #{env => #{dispatch => Dispatch}}).

routes(App, Handlers) ->
  Debug = cmkit:config(debug, App, false),
  State = #state{debug=Debug, handlers = Handlers},
  [
   {"/api/[...]", cmweb_cowboy, State},
   {"/ws", cmweb_cowboy, State},
   {"/", cowboy_static, {priv_file, App, "index.html"}},
   {"/[...]", cowboy_static, {priv_dir, App, "."}}
  ].

init(Req, State) ->
  case cowboy_req:path(Req) of
    <<"/ws">> -> 
      init_ws(Req, State);
    _ ->
      init_http(Req, State)
  end.

init_http(Req, #state{handlers=Handlers}=S) ->
  PathInfo = cowboy_req:path_info(Req),
  case cmweb_util:module(PathInfo, Handlers) of
    undefined ->
      cmweb_util:not_found(no_route, Req, S);
    Module ->
      case Module:spec() of 
        {data, Data} ->
          cmweb_util:ok(Data, Req, S);
        {data, Params, Fun} ->
          cmweb_util:with_input(Params, Req, fun(Input) ->
                                                 {ok, Data} = Fun(Input),
                                                 cmweb_util:ok(Data, Req, S)
                                             end);
        _ ->
          cmweb_util:not_impl(not_implemented, Req, S)
      end
  end.

init_ws(_Req, State) ->
  {ok, State}.
