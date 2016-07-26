-module(flactags).
-export([get_tags/1]).

-define(BLOCK_TYPE_STREAMINFO,     0).
-define(BLOCK_TYPE_PADDING,        1).
-define(BLOCK_TYPE_APPLICATION,    2).
-define(BLOCK_TYPE_SEEKTABLE,      3).
-define(BLOCK_TYPE_VORBIS_COMMENT, 4).
-define(BLOCK_TYPE_CUESHEET,       5).
-define(BLOCK_TYPE_PICTURE,        6).
-define(BLOCK_TYPE_INVALID,        127).

-define(BLOCK_HEADER_SIZE, 4).

get_tags(Filename) ->
    case file:open(Filename, [read, raw, binary]) of
        {ok, File} ->
            Tags = case check_file_header(File) of
                       true ->
                           find_blocks(File);
                       false ->
                           {error, not_flac} 
                   end,
            file:close(File),
            Tags;
        _ ->
            not_found
    end.

check_file_header(File) ->
    {ok, FileHeader} = file:read(File, 4),
    case FileHeader of
        <<"fLaC">> ->
            true;
        _ ->
            false
    end.

find_blocks(File) ->
    find_blocks(0, File, ?BLOCK_HEADER_SIZE, #{}).

find_blocks(0, File, Offset, Acc) ->
    file:position(File, Offset),
    {ok, Block} = file:read(File, ?BLOCK_HEADER_SIZE),
    {Last, BlockType, BlockLength} = parse_block_header(Block),
    case parse_blocks(BlockType, File, Offset, BlockLength, #{}) of
        {ok,Result} ->
            find_blocks(Last, File, Offset + BlockLength + ?BLOCK_HEADER_SIZE, maps:put(BlockType, Result, Acc));
        {skip,_} ->
            find_blocks(Last, File, Offset + BlockLength + ?BLOCK_HEADER_SIZE, Acc)
    end;
find_blocks(1, File, Offset, Acc) ->
    file:position(File, Offset),
    {ok, Block} = file:read(File, ?BLOCK_HEADER_SIZE),
    {_Last, BlockType, BlockLength} = parse_block_header(Block),
    case parse_blocks(BlockType, File, Offset, BlockLength, #{}) of
        {ok, Result} ->
            maps:put(BlockType, Result, Acc);
        {skip, _} ->
            Acc
    end.


%%%%
%% Internal Functions
%%%%

parse_block_header(<<Last:1, Type:7, Length:24>>) ->
    {Last, Type, Length}.

% Block Data Parser
% There we can add parsing any other blocks
parse_blocks(?BLOCK_TYPE_VORBIS_COMMENT, File, Offset, BlockLength, TagsMap) ->
    file:position(File, Offset + ?BLOCK_HEADER_SIZE),
    {ok, Block} = file:read(File, BlockLength),
    parse_block4_vn(<<Block/binary>>, TagsMap);
parse_blocks(BlockType, _, _, _, _) ->
    {skip, BlockType}.


parse_block4_vn(<<VNLen:4/little-signed-integer-unit:8, RestBlock4/binary>>, TagsMap) ->
    <<VendorName:VNLen/binary, _Skip:4/binary, _Block4Cutted/binary>> = RestBlock4,
    parse_block(<<_Block4Cutted/binary>>, maps:put(<<"VENDORNAME">>, VendorName, TagsMap)).

parse_block(<<VectorLen:4/little-signed-integer-unit:8, Block4/binary>>, TagsMap) ->
    <<Tag:VectorLen/binary, TagRest/binary>> = Block4,
    [Key, Val] = binary:split(Tag, <<"=">>),
    parse_block(<<TagRest/binary>>, maps:put(Key, Val, TagsMap));
parse_block(<<>>, TagsMap) ->
	{ok, TagsMap}.

