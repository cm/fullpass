-module(cmcsv).
-export([sample/2, parse/3]).

sample(File, Size) ->
    parse("/Users/pedrogutierrez/Downloads/" ++ File, Size, self()).

parse(File, BatchSize, Fun) ->
    case file:open(File, [raw, read_ahead, binary]) of
        {ok, IoDevice} ->
            case without_header(IoDevice) of
                {ok, _} ->
                    read_batch(IoDevice, BatchSize, BatchSize, [], Fun),
                    file:close(IoDevice),
                    ok;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end.

read_batch(IoDevice, 0, BatchSize, Batch, Fun) ->
    case Fun(Batch) of 
        ok -> 
            read_batch(IoDevice, BatchSize, BatchSize, [], Fun);
        _ -> 
            file:close(IoDevice),
            stopped
    end;

read_batch(IoDevice, Current, BatchSize, Batch, Fun) ->
    case file:read_line(IoDevice) of
        {ok, Data} ->
            Data2 = binary:part(Data, {0, size(Data)-1}),
            Fields = binary:split(Data2, <<",">>, [global]),
            read_batch(IoDevice, Current-1, BatchSize, [Fields|Batch], Fun);
        eof ->
            Fun(Batch),
            ok;
        Other -> 
            Other
    end.

without_header(IoDevice) ->
    file:read_line(IoDevice).
