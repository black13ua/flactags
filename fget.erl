-module(fget).
-export([get/1]).

-define(BLOCK_TYPE_STREAMINFO,     0).
-define(BLOCK_TYPE_PADDING,        1).
-define(BLOCK_TYPE_APPLICATION,    2).
-define(BLOCK_TYPE_SEEKTABLE,      3).
-define(BLOCK_TYPE_VORBIS_COMMENT, 4).
-define(BLOCK_TYPE_CUESHEET,       5).
-define(BLOCK_TYPE_PICTURE,        6).
-define(BLOCK_TYPE_INVALID,        127).

get(File) when is_list(File) ->
    case file:read_file(File) of
        {ok, Data} -> parse_bin(Data, [], 0);
        Any -> Any
    end.

parse_bin(Bin, Result, N) ->
    case Bin of
      <<"fLaC", Rest/binary>> ->
	io:format("This is FLAC file ~p~n", [[]]),
	parse_bin(<<Rest/binary>>, [], N + 1);
      <<0:1, Type:7, Length:24, Rest/binary>> ->
%	io:format("First metadata block is: ~p~n", [[Type, Length]]),
%	parse_bin(<<Rest/binary>>, <<>>, [], N + 1);
	cut_length(0, <<Rest/binary>>, Type, Length, Result);
      <<1:1, Type:7, Length:24, Rest/binary>> ->
	cut_length(1, <<Rest/binary>>, Type, Length, Result);
      <<>> ->
	lists:reverse(Result);
      Any -> error
    end.

cut_length(0, Rest, Type, Length, Result) ->
    io:format("Info for cutting ...~p~n", [[Type, Length]]),
    <<_Block:Length/binary, BinCutted/binary>> = Rest,
    parse_bin(<<BinCutted/binary>>, [{Type, Length}|Result], 0);
cut_length(1, Rest, Type, Length, Result) ->
    io:format("Last Block for cutting ...~p~n", [[Type, Length]]),
    <<_Block:Length/binary, BinCutted/binary>> = Rest,
    parse_bin(<<>>, [{Type, Length}|Result], 0).
