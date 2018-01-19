-module(cmcluster_create_backup).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"name">>}].

key() -> create_backup.

do(#{<<"name">> := Name}, S) ->
    case cmcluster_server:backup(Name) of 
        ok -> 
            {ok, create_backup, #{}, S};
        {error, E} -> 
            {error, E, S}
    end.
