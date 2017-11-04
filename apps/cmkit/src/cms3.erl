-module(cms3).
-export([put_file/3, get_file/2]).

config() ->
    AccessKey = cmkit:config(s3_access_key, cmweb),
    SecretKey = cmkit:config(s3_secret_key, cmweb),
    Host = cmkit:config(s3_host, cmweb),
    BucketAccess = cmkit:config(s3_bucket_access, cmweb),
    mini_s3:new(AccessKey, SecretKey, Host, BucketAccess).

put_file(Bucket, Key, Content) ->
    case mini_s3:put_object(Bucket, Key, Content, [], [], config()) of
        [{version_id, _}] -> ok;
        Error -> {error, Error}
    end.

get_file(Bucket, Key) ->
    case mini_s3:get_object(Bucket, Key, [], config()) of
        [_|_]=Meta ->
            case proplists:get_value(content, Meta) of
                undefined ->
                    {error, no_content};
                Data ->
                    {ok, Data}
            end;
        Other ->
            {error, Other}
    end.
