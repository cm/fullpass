-module(cmcsv).
-export([sample/2, fold/3, parse/3]).

sample(File, Size) ->
    parse("/Users/pedrogutierrez/Downloads/" ++ File, Size, self()).

fold(File, Acc, Fun) ->
    case file:open(File, [{encoding, utf8}, read_ahead, binary]) of
        {ok, IoDevice} ->
            case without_header(IoDevice) of
                {ok, _} ->
                    R = fold_line(IoDevice, 1, Acc, Fun),
                    file:close(IoDevice),
                    R;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end.

fold_line(Io, LinesRead, Acc, Fun) ->
    case file:read_line(Io) of
        {ok, Data} ->
            Data2 = binary:part(Data, {0, size(Data)-1}),
            Fields = binary:split(Data2, <<",">>, [global]),
            Acc2 = Fun(Acc, Fields),
            fold_line(Io, LinesRead+1, Acc2, Fun);
        eof ->
            {ok, Acc, LinesRead +1};
        Other -> 
            Other
    end.


parse(File, BatchSize, Fun) ->
    case file:open(File, [raw, read_ahead, binary]) of
        {ok, IoDevice} ->
            case without_header(IoDevice) of
                {ok, _} ->
                    R = read_batch(IoDevice, BatchSize, BatchSize, 0, [], Fun),
                    file:close(IoDevice),
                    R;
                Other -> 
                    Other
            end;
        Other -> 
            Other
    end.

read_batch(IoDevice, 0, BatchSize, LinesRead, Batch, Fun) ->
    case Fun(Batch) of 
        ok -> 
            read_batch(IoDevice, BatchSize, BatchSize, LinesRead, [], Fun);
        Error ->
            file:close(IoDevice),
            Error
    end;

read_batch(IoDevice, Current, BatchSize, LinesRead, Batch, Fun) ->
    case file:read_line(IoDevice) of
        {ok, Data} ->
            Unicode = cmkit:uniconvert(binary_to_list(Data)), 
            Data2 = binary:part(Unicode, {0, size(Unicode)-1}),
            Fields = binary:split(Data2, <<",">>, [global]),
            read_batch(IoDevice, Current-1, BatchSize, LinesRead + 1, [Fields|Batch], Fun);
        eof ->
            Fun(Batch),
            {ok, LinesRead +1};
        Other -> 
            Other
    end.

without_header(IoDevice) ->
    file:read_line(IoDevice).
