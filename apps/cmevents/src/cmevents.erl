-module(cmevents).
-export([cmdb_tables/0, cmdb_init/0]).
-export([behaviour_info/1, register_plugins/1]). 

cmdb_tables() -> 
    [{event_plugins, set, ram_copies}].

behaviour_info(callbacks) ->
    [{cmevents_plugins, 0}].

cmdb_init() ->
    lists:flatten([ register_plugins(M) || M <-erlang:loaded(), cmkit:implements(M, behaviour_info(callbacks)) ]).

register_plugins(M) ->
    [ {i, event_plugins, {Event, Type}, is, {Mod, Tab, Rel, InverseRel}}
        || {Mod, Event, Type, Tab, Rel, InverseRel} <- M:cmevents_plugins() ].
