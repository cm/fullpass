-module(cmrtc).
-export([cmdb_tables/0]).
-export([ws_init/2]).

ws_init(SessId, Conn) ->
    cmcluster:dispatch(session, {start, SessId, Conn}).

cmdb_tables() ->
    [].
