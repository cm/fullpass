-module(facebook).
-export([
  login/3,
  events/3
]).

login(Code, Ok, Err) ->
  access_token(Code, fun(T, _Exp) ->
    profile(T, Ok, Err)
  end, Err).

access_token(Code, Ok, Err) ->
  q(access_token_url(Code), fun(R) ->
    #{<<"access_token">> := T, <<"expires_in">> := Exp} = R,
    Ok(T, Exp)
  end, Err).

profile(Token, Ok, Err) ->
  q(profile_url(Token), fun(Profile) ->
    q(picture_url(Profile, Token), fun(#{ <<"data">> := Picture}) ->
      Ok(maps:merge(maps:put(<<"picture">>, maps:get(<<"url">>, Picture), Profile), #{ <<"access_token">> => Token}))    
    end, Err)
  end, Err).

events(Profile, Ok, Err) ->
  q(events_url(Profile), fun(#{<<"data">> := Events}) ->
    Ok(Events)
  end, Err).

access_token_url(Code) ->
  cmkit:fmt("~s/v2.3/oauth/access_token?client_id=~s&redirect_uri=~s&client_secret=~s&code=~s", 
  [cmkit:config(url, ?MODULE),
   cmkit:config(client, ?MODULE),
   cmkit:config(redirect, ?MODULE),
   cmkit:config(secret, ?MODULE),
   Code]).

profile_url(Token) ->
  cmkit:fmt("~s/me?fields=id,first_name,last_name,picture&access_token=~s", 
  [ cmkit:config(url, ?MODULE),
    Token
  ]).

picture_url(#{<<"id">> := Id}, Token) ->
  cmkit:fmt("~s/~s/picture?type=large&redirect=0&access_token=~s", 
  [ cmkit:config(url, ?MODULE),
    Id,
    Token
  ]).

events_url(#{<<"id">> := Id, <<"access_token">> := T}) ->
  cmkit:fmt("~s/~s/events?since=~w&access_token=~s", 
  [ cmkit:config(url, ?MODULE),
    Id,
    cmkit:now(),
    T
  ]).


q(Url, Ok, Err) ->
  case httpc:request(binary_to_list(Url)) of
    {ok, {{_, 200, _}, _, B}}  -> 
      case cmkit:jsond(B) of
        {error, E} -> Err(E);
        Json -> Ok(Json)
      end;
    {ok, {{_, S, _}, _, B}}  -> 
      io:format( "Got error from facebook: ~p~n", [B]),
      Err(S);
    _  -> 
      Err(<<"Unknown error while querying facebook">>)
  end.
