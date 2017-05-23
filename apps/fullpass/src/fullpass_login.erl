-module(fullpass_login).
-behaviour(cmaggregate).
-export([init/0, handle/2, topic/0]).

topic() ->
  {any_worker, login}.

init() -> [].

handle({login, Code, From}=Msg, _) ->
  facebook:login(Code, fun(Profile) ->
    cmcluster:event({profile, Profile, From})
  end, fun(Error) ->
    cmcluster:err(Msg, Error)
  end).
