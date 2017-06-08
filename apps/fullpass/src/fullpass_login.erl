-module(fullpass_login).
-behaviour(cmplugin).
-export([init/1, data/2, key/1, missing/1]).

key(_) -> login.

init(_) -> [].

data([Code, From], Data) ->
  facebook:login(Code, fun(Profile) ->
    cmdb:cast(profile, [Profile, From]),
    {ok, Data}
  end, fun(Error) ->
    cmdb:cast(error, Error),
    {ok, Data}
  end).

missing(_) -> ignore.
