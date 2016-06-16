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
        {ok, Data} -> check_flac_file(Data);
        Any -> Any
    end.

check_flac_file(<<"fLaC", Rest/binary>>) ->
    io:format("This is FLAC file ~p~n", [?MODULE]),
    parse_bin(<<Rest/binary>>, []).

parse_bin(Bin, Result) ->
    case Bin of
        %% Find some blocks in header
        <<0:1, Type:7, Length:24, Rest/binary>> ->
            cut_length(0, <<Rest/binary>>, Type, Length, Result);
        %% Find last block in header
        <<1:1, Type:7, Length:24, Rest/binary>> ->
            cut_length(1, <<Rest/binary>>, Type, Length, Result);
        <<>> ->
            lists:reverse(Result);
        _ -> error
    end.

cut_length(0, Rest, ?BLOCK_TYPE_VORBIS_COMMENT=Type, Length, Result) ->
    io:format("Block 4 FOUND ...~p~n", [[Type, Length]]),
    <<_Block:Length/binary, _BinCutted/binary>> = Rest,
    %parse_block(<<_Block:NLength/binary>>, []),
    parse_block4_vn(<<_Block:Length/binary>>, []),
    %%%% Block 4 - That all what i need
    parse_bin(<<>>, [{Type, Length}|Result]);
    %parse_bin(<<BinCutted/binary>>, [{Type, Length}|Result], 0);
cut_length(0, Rest, Type, Length, Result) ->
    %io:format("Info for cutting ...~p~n", [[Type, Length]]),
    <<_Block:Length/binary, BinCutted/binary>> = Rest,
    parse_bin(<<BinCutted/binary>>, [{Type, Length}|Result]);
cut_length(1, Rest, Type, Length, Result) ->
    io:format("Last Block for cutting ...~p~n", [[Type, Length]]),
    <<_Block:Length/binary, _BinCutted/binary>> = Rest,
    parse_bin(<<>>, [{Type, Length}|Result]).

parse_block4_vn(<<VNLen:4/little-signed-integer-unit:8, RestBlock4/binary>>, _List) ->
    <<_VendorName:VNLen/binary, _Skip:4/binary, _Block4Cutted/binary>> = RestBlock4,
    parse_block(<<_Block4Cutted/binary>>, [_VendorName]).
    %io:format("As Pretty VLen: ~p~n", [VNLen]),
    %io:format("As Pretty VendorName: ~p~n", [_VendorName]).

parse_block(<<VectorLen:4/little-signed-integer-unit:8, Block4/binary>>, TagsList) ->
    %io:format("As Pretty Len: ~p~n", [VectorLen]),
    <<Tag:VectorLen/binary, TagRest/binary>> = Block4,
    %io:format("As Pretty Tag: ~p~n", [Tag]),
    %io:format("As Pretty Tag: ~p~n", [TagRest]),
    parse_block(<<TagRest/binary>>, [Tag|TagsList]);
parse_block(<<>>, TagsList) ->
    io:format("Result: ~p~n", [lists:reverse(TagsList)]).
