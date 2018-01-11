-module(cmcluster_start_db).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"host">>}].

key() -> start_db.

do(#{<<"host">> := Host}, S) ->
    case cmcluster:start_db(Host) of
        ok -> 
            {ok, start_db, #{host => Host}, S};
        {error, E} ->
            {error, E, S}
    end.
