-module(cmweb_util).
-export([module/2, ok/3, not_found/3, not_impl/3, invalid/3, forbidden/3, err/3, redirect/4]).

module(K, Entries) ->
  case maps:is_key(K, Entries) of
    false ->
      undefined;
    true ->
      maps:get(K, Entries)
  end.

ok(Data, Req, State) ->
  {ok, reply(200, Data, [], Req), State}.

not_found(Reason, Req, State) ->
  {ok, reply(404, #{reason => Reason}, [], Req), State}.

not_impl(Reason, Req, State) ->
  {ok, reply(501, #{reason => Reason}, [], Req), State}.

invalid(Reason, Req, State) ->
  {ok, reply(400, #{reason => Reason}, [], Req), State}.

forbidden(Reason, Req, State) ->
  {ok, reply(403, #{reason => Reason}, [], Req), State}.

err(Reason, Req, State) ->
  {ok, reply(500, #{reason => Reason}, [], Req), State}.

redirect(Loc, Cookies, Req, State) ->
  Req2 = maps:fold(fun(K, V, R) ->
    cowboy_req:set_resp_cookie(K, V, R, #{path => <<"/">>})
  end, Req, Cookies),
  {ok, cowboy_req:reply(303, #{
    <<"location">> => Loc
  }, Req2), State}.

reply(Status, Body, _Headers, Req) ->
  cowboy_req:reply(Status, #{
                           <<"content-type">> => <<"application/json">>
                           }, jiffy:encode(Body), Req).
