-module(cmcluster_stop_db).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"host">>}].

key() -> stop_db.

do(#{<<"host">> := Host}, S) ->
    case cmcluster:stop_db(Host) of
        ok -> 
            {ok, stop_db, #{host => Host}, S};
        {error, E} ->
            {error, E, S}
    end.
