-module(fullpass_profile).
-behaviour(cmaggregate).
-export([init/0, handle/2, topic/0, worker_topic/1]).
-record(data, {profile, sessions}}

topic() ->
  {one_worker, profile}.

worker_topic({profile, #{<<"id">> := Id}, _}) ->
  {profile, Id}.

init() -> .

handle({profile, P, From}=Msg, _) ->
  facebook:login(Code, fun(Profile) ->
    cmcluster:event({profile, Profile, From})
  end, fun(Error) ->
    cmcluster:err(Msg, Error)
  end).
