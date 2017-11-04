-module(cmevents_event).
-export([spec/0, key/0, do/2]).

spec() -> [{text, <<"id">>},
           {text, <<"type">>},
           {text, <<"event">>}].

key() -> event.

do(_, S) when map_size(S) == 0 ->
    {error, forbidden, S};

do(#{<<"event">> := Event, 
     <<"id">> := Id, 
     <<"type">> := Type}, #{id := Uid}=S) ->
    case cmdb:r(event_plugins, {Event, Type}, is) of
        {ok, {Mod, Tab, Rel, InverseRel}} ->
                    Entries = [{u, Tab, Uid, Rel, Id},
                               {u, Tab, Id, InverseRel, Uid}],
                    case Mod:event(Id, S, Event, Entries) of
                        ok -> 
                            EventData = #{ user => Uid,
                                           id => Id,
                                           type => Type,
                                           event => Event },
                            {ok, Event, EventData, S};
                        {error, E} ->
                            {error, E, S}
                    end;
        not_found -> 
            {error, not_found, S};
        {error, Error} -> 
            {error, Error}
    end.
