-module(fullpass_login).
-behaviour(cmaggregate).
-export([mode/0, init/1, handle/2, topic/0, topic/1, missing/1]).

mode() ->
  any_worker.

topic() ->
  login.

topic(_) ->
  login.

init(_) -> [].

handle({login, Code, From}=Msg, Data) ->
  facebook:login(Code, fun(Profile) ->
    cmcluster:event({profile, Profile, From}),
    {ok, Data}
  end, fun(Error) ->
    cmcluster:err(Msg, Error),
    {ok, Data}
  end).

missing(_) -> ignore.
