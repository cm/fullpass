-module(cmweb_util).
-export([module/2, 
         cors/3,
         file/1,
         ok/3, 
         ok/4, 
         not_found/3, 
         not_impl/3, 
         invalid/3, 
         forbidden/3, 
         err/3, 
         redirect/4,
         cookie/2,
         cookie/3,
         ws_ok/3,
         ws_ack/2,
         ws_invalid/2,
         ws_invalid/3,
         ws_not_found/2,
         ws_error/3,
         ws_error_with_reason/3,
         log/2]).

module(K, Entries) ->
  case maps:is_key(K, Entries) of
    false ->
      undefined;
    true ->
      maps:get(K, Entries)
  end.

cors(Methods, Headers, Req0) ->
    Req1 = cowboy_req:set_resp_header(
        <<"access-control-allow-methods">>, Methods, Req0),
    Req2 = cowboy_req:set_resp_header(
        <<"access-control-allow-origin">>, <<"*">>, Req1),
    Req3 = cowboy_req:set_resp_header(
        <<"access-control-allow-headers">>, Headers, Req2),
    Req3.

file(Req) ->
    case cowboy_req:has_body(Req) of
        false -> 
            {error, missing_body};
        true ->
            case cowboy_req:parse_header(<<"content-type">>, Req) of
                {<<"multipart">>, <<"form-data">>, _} ->
                    case cowboy_req:read_part(Req) of
                        {ok, PartHeaders, Req2} -> 
                            case cowboy_req:read_part_body(Req2,  #{ length => 256000 })  of
                                {more, _, _Req3} ->
                                    {error, too_big};
                                {ok, Data, Req3} -> 
                                    FileId = cmkit:uuid(),
                                    case cms3:put_file("netcomposer-files", binary_to_list(FileId), Data) of
                                        ok -> 
                                            Size = byte_size(Data),
                                            {file, <<"file">>, Name, Type} = cow_multipart:form_data(PartHeaders),
                                            {ok, #{ name => Name,
                                                    type => Type,
                                                    id => FileId,
                                                    size => Size }, Req3};
                                        {error, Error} ->
                                            {error, Error}
                                    end
                            end;
                        Other ->
                            cmkit:log({cmweb, unexpected_part, Other}),
                            {error, missing_part}
                    end;
                Other ->
                    cmkit:log({cmweb, unexpected_part, Other}),
                    {error, missing_part}
            end
    end.

ok(Data, Req, State) ->
  {ok, reply(200, Data, [], Req), State}.

ok(Action, Data, Req, State) ->
  {ok, reply(200, #{ action => Action, 
                     data => Data,
                     status => <<"ok">> }, [], Req), State}.

not_found(Reason, Req, State) ->
  {ok, reply(404, #{reason => Reason}, [], Req), State}.

not_impl(Reason, Req, State) ->
  {ok, reply(501, #{reason => Reason}, [], Req), State}.

invalid(Reason, Req, State) ->
  {ok, reply(400, #{reason => Reason}, [], Req), State}.

forbidden(Reason, Req, State) ->
  {ok, reply(403, #{reason => Reason}, [], Req), State}.

err(Reason, Req, State) ->
  {ok, reply(500, #{reason => Reason}, [], Req), State}.

redirect(Loc, Cookies, Req, State) ->
  Req2 = maps:fold(fun(K, V, R) ->
    cowboy_req:set_resp_cookie(K, V, R, #{path => <<"/">>})
  end, Req, Cookies),
  {ok, cowboy_req:reply(303, #{
    <<"location">> => Loc
  }, Req2), State}.

reply(Status, Body, _Headers, Req) ->
    cowboy_req:reply(Status, #{
                           <<"content-type">> => <<"application/json">>
                           }, cmkit:jsone(Body), Req).

cookie(Name, Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    case lists:keyfind(Name, 1, Cookies) of
        {_, Value} -> Value;
        false -> undefined 
    end.

cookie(Input, Name, Req) ->
    case cookie(Name, Req) of
        undefined -> Input;
        Value ->
            maps:put(Name, Value, Input)
    end.

ws_ok(Data, Action, State) ->
    ws_reply(Data, <<"ok">>, Action, State).


ws_ack(Action, State) ->
    ws_reply(<<"ack">>, Action, State).

ws_error(Data, Action, State) ->
    ws_reply(Data, <<"error">>, Action, State).

ws_error_with_reason(R, Action, State) -> 
    ws_reply(#{reason => R}, <<"error">>, Action, State).

ws_invalid(R, Action, State) ->
    ws_reply(R, <<"invalid">>, Action, State).

ws_invalid(Action, State) ->
    ws_reply(#{}, <<"invalid">>, Action, State).

ws_not_found(Action, State) ->
    ws_reply(#{}, <<"not_found">>, Action, State).

ws_reply(Reply, State) ->
    log(ws_out, Reply),  
    {reply, {text, cmkit:jsone(Reply)}, State}.

ws_reply(Status, Action, State) ->
  ws_reply(#{<<"action">> => Action,
             <<"status">> => Status }, State).

ws_reply(Data, Status, Action, State) ->
  ws_reply(#{ <<"data">> => Data,
             <<"action">> => Action,
             <<"status">> => Status }, State).



log(_, #{<<"action">> := <<"pong">>}) -> ok;
log(_, #{<<"action">> := <<"ping">>}) -> ok;
log(Channel, Data) -> cmkit:log({Channel, Data}).
