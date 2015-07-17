-module(flactags).
-export([get_tags/1]).

get_tags(Filename) when is_list(Filename) ->
    case file:read_file(Filename) of
        {ok, Data} -> parse(Data);
        Any -> Any
    end.

parse(<<"fLaC", Data/binary>>) ->
    io:format("This is Flac file! ~p~n", [?MODULE]),
    parse_meta_blocks(Data, 256, <<>>).

parse_meta_blocks(<<>>, _N, BinAcc) ->
    io:format("Data Done! ~p~n", [?MODULE]),
    BinAcc;
parse_meta_blocks(_, 0, BinAcc) ->
    io:format("N Done! ~p~n", [?MODULE]),
    BinAcc;
parse_meta_blocks(<<Byte:4/binary, Tail/binary>>, N, BinAcc) ->
    io:format("As Term: ~w, ", [Byte]),
    io:format("As String: ~s, ", [Byte]),
    io:format("Data Parse Byte! ~p~n", [[Byte, N]]),
    parse_meta_blocks(<<Tail/binary>>, N - 1, <<BinAcc/binary, Byte/binary>>).
