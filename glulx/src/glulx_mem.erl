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
-export([read_file/1, header/1, get_byte/2, get_word16/2, get_word32/2]).
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

% Extract the Glulx header information
% @spec header(GlulxMemory()) -> record().
header({BaseMemory, _ExtMemory}) -> header_private(BaseMemory).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Helper functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Returns the value with the specified number of bits from the binary
%% @spec get_bits(binary(), int(), int()) -> int().
get_bits({BaseMemory, ExtMemory}, ByteNum, NumBits) ->
    if 
	ByteNum >= size(BaseMemory) ->
	    Address = ByteNum - size(BaseMemory),
	    MemChunk = ExtMemory;
	true ->
	    Address = ByteNum,
	    MemChunk = BaseMemory
    end,
    <<_:Address/binary, Element:NumBits, _/binary>> = MemChunk,
    Element.

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
