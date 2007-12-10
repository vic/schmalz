%%% This file is part of Schmalz.
%%%
%%% Schmalz is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% Schmalz is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with Schmalz.  If not, see <http://www.gnu.org/licenses/>.

%%%-----------------------------------------------------------------------
%%% Description of module glulx_mem
%%%-----------------------------------------------------------------------
%%% This is the memory module of the Glulx interpreter, most other modules
%%% rely on the functions in this module to read and write data to the
%%% story memory. GluLx memory is divided into base and extended memory,
%%% so there are two separate chunks that are managed by this module.
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%%
%%% read_file(Filename)
%%%   reads the specified file and returns a chunk of memory
%%%
%%% header(Memory)
%%%   retrieves the header information
%%%
%%%-----------------------------------------------------------------------

-module(glulx_mem).
-export([read_file/1, header/1, memsize/1,
	 get_byte/2, get_word16/2, get_word32/2, get_ram_word32/2,
	 set_byte/3, set_word32/3, set_ram_word32/3,
	 get_bit/3]).
-include("include/glulx.hrl").

%% reads the game from the specified file and returns a Memory object
%% @spec read_file(string()) -> GlulxMemory().
read_file(Filename) ->
    {_Status, BaseMemory} = file:read_file(Filename),
    Header = header_private(BaseMemory),
    SizeExtMem = Header#glulx_header.end_mem - Header#glulx_header.ext_start,
    ExtMemory =
	case SizeExtMem of
	    0 -> <<>>;
	    _Default -> list_to_binary([ 0 || _ <- lists:seq(1, SizeExtMem)])
	end,
    {BaseMemory, ExtMemory}.

%% Extract the Glulx header information
%% @spec header(GlulxMemory()) -> record().
header({BaseMemory, _ExtMemory}) -> header_private(BaseMemory).

%% Returns the size of total memory.
%% @spec memsize(GlulxMem()) -> int().
memsize({BaseMemory, ExtMemory}) -> size(BaseMemory) + size(ExtMemory).

%% Returns the byte at the specified address
%% @spec get_byte(GlulxMemory(), int()) -> int().
get_byte(Memory, ByteNum) ->
    get_bits(Memory, ByteNum, 8).

%% Returns the 16 bit word at the specified address
%% @spec get_word16(GlulxMemory(), int()) -> int().
get_word16(Memory, ByteNum) ->
    get_bits(Memory, ByteNum, 16).

%% Returns the 32 bit word at the specified address
%% @spec get_word32(GlulxMemory(), int()) -> int().
get_word32(Memory, ByteNum) ->
    get_bits(Memory, ByteNum, 32).

%% Returns the 32 bit word at the specified ram offset
%% @spec get_ram_word32(GlulxMemory(), int()) -> int().
get_ram_word32(Memory, RamOffset) ->
    Header = header(Memory),
    get_bits(Memory, Header#glulx_header.ram_start + RamOffset, 32).

set_byte(Memory, ByteNum, Value) -> set_bits(Memory, ByteNum, 8, Value).
set_word32(Memory, ByteNum, Value) -> set_bits(Memory, ByteNum, 32, Value).

set_ram_word32(Memory, RamOffset, Value) ->
    Header = header(Memory),
    set_bits(Memory, Header#glulx_header.ram_start + RamOffset, 32, Value).

%% @spec get_bit(binary(), int(), int()) -> int().
get_bit(Memory, ByteNum, BitIndex) ->
    RemainBits = 8 - (BitIndex + 1),
    {_, MemChunk, Address} = memchunk_address(Memory, ByteNum),
    <<_:Address/binary, _:RemainBits, Bit:1, _:BitIndex, _/binary>> = MemChunk,
    Bit.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Helper functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Returns the value with the specified number of bits from the binary
%% @spec get_bits(binary(), int(), int()) -> int().
get_bits(Memory, ByteNum, NumBits) ->
    {_, MemChunk, Address} = memchunk_address(Memory, ByteNum),
    <<_:Address/binary, Element:NumBits, _/binary>> = MemChunk,
    Element.

% Sets the Value at the specified byte position
%% @spec set_bits(binary(), int(), int(), int()) -> binary().
set_bits({BaseMemory, ExtMemory} = Memory, ByteNum, NumBits, Value) ->
    {ChunkType, MemChunk, ChunkAddress} = memchunk_address(Memory, ByteNum),
    <<Start:ChunkAddress/binary, _:NumBits, End/binary>> = MemChunk,
    NewMemChunk = <<Start:ChunkAddress/binary, Value:NumBits, End/binary>>,
    case ChunkType of
	basemem  -> {NewMemChunk, ExtMemory};
	_Default -> {BaseMemory, NewMemChunk}
    end.

%% Determines the memory chunk and chunk address
memchunk_address({BaseMemory, ExtMemory}, ByteNum) ->
    %io:format("memchunk_address(), ByteNum: ~w~n", [ByteNum]),
    if 
	ByteNum >= size(BaseMemory) ->
	    {extmem, ExtMemory, ByteNum - size(BaseMemory)};
	true ->
	    {basemem, BaseMemory, ByteNum}
    end.

% Extract the Glulx header information
% @spec header_private(binary()) -> record().
header_private(Memory) ->
    <<MagicNumber:32, Major:16, Minor:8, Subminor:8, RamStart:32,
      ExtStart:32, EndMem:32, StackSize:32, StartFunc:32, DecodeTable:32,
      CheckSum:32, _/binary>> = Memory,
    #glulx_header{magic_number = MagicNumber,
		  version = {Major, Minor, Subminor},
		  ram_start = RamStart, ext_start = ExtStart, end_mem = EndMem,
		  stack_size = StackSize, start_func = StartFunc,
		  decode_table = DecodeTable, checksum = CheckSum}.
