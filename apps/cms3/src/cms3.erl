-module(cms3).
-export([config/0, put_file/3, get_file/2]).
-define(APP, cms3).

config() ->
    {cmkit:config(s3_access_key, ?APP),
     cmkit:config(s3_secret_key, ?APP),
     cmkit:config(s3_host, ?APP),
     cmkit:config(s3_port, ?APP),
     cmkit:config(s3_scheme, ?APP),
     cmkit:config(s3_region, ?APP),
     cmkit:config(s3_bucket_access, ?APP)}.

client() ->
    {AccessKey, SecretKey, Host, Port, Scheme, _Region, BucketAccess} = config(),
    Endpoint = atom_to_list(Scheme) ++ "://" ++ Host ++ ":" ++ integer_to_list(Port),
    cmkit:log({cms3, Endpoint}),
    mini_s3:new(AccessKey, SecretKey, Endpoint, BucketAccess).

put_file(Bucket, Key, Content) ->
    case mini_s3:put_object(Bucket, Key, Content, [], [], client()) of
        [{version_id, _}] -> ok;
        Error -> {error, Error}
    end.

get_file(Bucket, Key) ->
    case mini_s3:get_object(Bucket, Key, [], client()) of
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
