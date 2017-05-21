-module(fullpass_login).
-behaviour(cmaggregate).
-export([init/0, handle/1, topic/0]).

topic() ->
  {worker, login, any}.

init() ->
  ok.

handle({login, Code, From}=Msg) ->
  facebook:login(Code, fun(Profile) ->
    cmcluster:event({profile, Profile, From})
  end, fun(Error) ->
    cmcluster:err(Msg, Error)
  end).
