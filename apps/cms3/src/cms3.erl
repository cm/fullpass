-module(cms3).
-export([config/0, put_file/3, get_file/2]).
-include_lib("erlcloud/include/erlcloud_aws.hrl").
-define(APP, cms3).

config() ->
    {cmkit:config(s3_access_key, ?APP),
     cmkit:config(s3_secret_key, ?APP),
     cmkit:config(s3_host, ?APP),
     cmkit:config(s3_timeout, ?APP)
     }.

client() -> 
    {AccessKey, SecretKey, Host, Timeout} = config(),
    Cfg = erlcloud_s3:new(AccessKey, SecretKey, Host),
    Cfg#aws_config{timeout=Timeout}.

put_file(Bucket, Key, Content) ->
    case erlcloud_s3:put_object(Bucket, Key, Content, client()) of 
        [{version_id, _}|_] -> ok;
        Error -> {error, Error}
    end.
    
get_file(Bucket, Key) ->
    try erlcloud_s3:get_object(Bucket, Key, client()) of
        [_|_]=Meta ->
            case proplists:get_value(content, Meta) of
                undefined ->
                    {error, no_content};
                Data ->
                    {ok, Data}
            end;
        Other ->
            {error, Other}

    catch
        _:{aws_error, {http_error, 404, _, _}} -> 
            {error, not_found};
        _:E -> 
            {error, E}

    end.
