-module(cmrtc_capabilities).
-export([spec/0, key/0, do/2]).

spec() -> 
    [{text, <<"group">>},
     {text, <<"sdp">>}].

key() -> rtc_capabilities.

do(_, S) when map_size(S) == 0 ->
    {error, forbidden, S};

do(#{<<"sdp">>:= Sdp, 
     <<"group">> := Gid},  #{id:=Uid}=S) ->
    cmsoup_client:out(#{ action => join,
                         room => Gid,
                         participant => Uid,
                         sdp => Sdp }),
    {noreply, S}.
