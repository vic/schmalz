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
%%% Description of module memory
%%%-----------------------------------------------------------------------
%%% This is the base module of the Z code interpreter, most other modules
%%% rely on the functions in this module to read and write data to the
%%% story memory. 
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%%
%%% read_file(Filename)
%%%   reads the specified file and returns a chunk of memory
%%%
%%% version(Memory)
%%%   determines the story file version
%%%
%%% dictionary_address(Memory)
%%%   determines the address of the dictionary
%%%
%%% object_table_address(Memory)
%%%   determines the address of the object table
%%%
%%% abbrev_address(Memory)
%%%   determines the address of the abbreviations table
%%%
%%% initial_pc(Memory)
%%%   determines the initial program counter
%%%
%%% global_var_address(Memory)
%%%   determines the start address of the global variables
%%%
%%% unpack_address(Memory, PackedAddress)
%%%   converts a packed address to a byte address
%%%
%%%-----------------------------------------------------------------------
-module(memory).
-vsn('1.0').
-created_by('Wei-ju Wu').
-export([read_file/1,
	 version/1, object_table_address/1, dictionary_address/1,
	 global_var_address/1, abbrev_address/1, initial_pc/1,
	 unpack_address/2, get_byte/2, get_word16/2, set_byte/3, set_word16/3,
	 get_bytes/3, copy_string_to_address/3]).

% Fixed offsets
-define(VERSION,        16#00).
-define(INITIAL_PC,     16#06).
-define(DICTIONARY,     16#08).
-define(OBJECTS,        16#0A).
-define(GLOBAL_VARS,    16#0C).
-define(ABBREVIATIONS,	16#18).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% memory contains the methods to manipulate and access the IF
%%% game's story file. In Erlang as a functional language, the story
%%% file's memory state is transformed by the functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% reads the game from the specified file and returns a Memory object
%% @spec read_file(string()) -> binary().
read_file(Filename) ->
    {_Status, Memory} = file:read_file(Filename),
    Memory.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Story file header
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% returns thhe story file version
%% @spec version(binary()) -> int().
version(Memory) -> memory:get_byte(Memory, ?VERSION).

%% returns the address of the dictionary
%% @spec dictionary_address(binary()) -> int().
dictionary_address(Memory) -> memory:get_word16(Memory, ?DICTIONARY).

%% returns the address of the object table
%% @spec object_table_address(binary()) -> int().
object_table_address(Memory) -> memory:get_word16(Memory, ?OBJECTS).

%% returns the address of the abbreviations table
%% @spec abbrev_address(binary()) -> int().
abbrev_address(Memory) -> memory:get_word16(Memory, ?ABBREVIATIONS).

%% returns the initial program counter
%% @spec initial_pc(binary()) -> int().
initial_pc(Memory) -> memory:get_word16(Memory, ?INITIAL_PC).

%% returns the address of the global variables
%% @spec global_var_address(binary()) -> int().
global_var_address(Memory) -> memory:get_word16(Memory, ?GLOBAL_VARS).

%% Unpacks a packed address, works for versions 3-5 and 8.
%% Version 6 and 7 have two different unpack formulas and are not handled
%% here.
%% @spec unpack_address(binary(), int()) -> int(). 
unpack_address(Memory, PackedAddress) ->
    case version(Memory) of
	3        -> PackedAddress * 2;
	4        -> PackedAddress * 4;
	5        -> PackedAddress * 4;
	8        -> PackedAddress * 8;
	_Default -> undef
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generic binary access functionality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

copy_string_to_address(Memory, Address, []) ->
    set_byte(Memory, Address, 0);
copy_string_to_address(Memory, Address, [Character|String]) ->
    copy_string_to_address(set_byte(Memory, Address, Character), Address + 1,
			   String).

% reads the specified number of bytes from the given position
get_bytes(Memory, ByteNum, NumBytes) ->
    <<_:ByteNum/binary, Result:NumBytes/binary, _/binary>> = Memory, Result.

%% @spec get_word16(binary(), int()) -> int().
get_word16(Memory, ByteNum) -> get_bits(Memory, ByteNum, 16).

%% @spec set_word16(binary(), int(), int()) -> binary().
set_word16(Memory, ByteNum, Value) -> set_bits(Memory, ByteNum, 16, Value).

%% @spec get_byte(binary(), int()) -> int().
get_byte(Memory, ByteNum) -> get_bits(Memory, ByteNum, 8).

%% @spec set_byte(binary(), int(), int()) -> binary().
set_byte(Memory, ByteNum, Value) -> set_bits(Memory, ByteNum, 8, Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Sets the Value at the specified byte position
%% @spec set_bits(binary(), int(), int(), int()) -> binary().
set_bits(Memory, ByteNum, NumBits, Value) ->
    <<Start:ByteNum/binary, _:NumBits, End/binary>> = Memory,
    <<Start:ByteNum/binary, Value:NumBits, End/binary>>.

% Returns the value with the specified number of bits from the binary
%% @spec get_bits(binary(), int(), int()) -> int().
get_bits(Memory, ByteNum, NumBits) ->
    <<_:ByteNum/binary, Element:NumBits, _/binary>> = Memory, Element.
