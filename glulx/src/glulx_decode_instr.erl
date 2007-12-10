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
%%% Description of module glulx_decode_instr
%%%-----------------------------------------------------------------------
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%%
%%%-----------------------------------------------------------------------

-module(glulx_decode_instr).
-export([decode/1]).
-include("include/glulx.hrl").

decode(MachinePid) ->
    Address = ?call_machine(pc),
    First = ?get_byte(Address),
%    Masked = First band 2#11000000,
    %io:format("byte at $~8.16.0B: ~w, masked: ~16.2.0B~n",
%	      [Address, First, Masked]),
    case First band 2#11000000 of
	2#11000000 ->
	    OpNumLen = 4,
	    OpcodeNum = ?get_word32(Address) - 16#c0000000;
	2#10000000 ->
	    OpNumLen = 2,
	    OpcodeNum = ?get_word16(Address) - 16#8000;
	_Default ->
	    OpNumLen = 1,
	    OpcodeNum = First
    end,
    %io:format("$~8.16.0B: OpcodeNum: #$~8.16.0B~n", [Address, OpcodeNum]),
    {OperandDataSize, Operands} = decode_operands(MachinePid,
						  Address + OpNumLen,
						  OpcodeNum),
    #instr{opcode = OpcodeNum, operands = Operands, address = Address,
	   opnum_len = OpNumLen, length = OpNumLen + OperandDataSize}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Helper fuctionality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_operands(MachinePid, Address, OpcodeNum) ->
    NumOperands = case OpcodeNum of
	?ADD          -> 3;
	?ALOAD        -> 3;
	?ALOADB       -> 3;
	?ALOADBIT     -> 3;
	?ALOADS       -> 3;
	?ASTORE       -> 3;
	?ASTOREBIT    -> 3;
	?BINARYSEARCH -> 8;
	?BITAND       -> 3;
	?CALL         -> 3;
	?COPY         -> 2;
	?COPYB        -> 2;
	?CALLF        -> 2;
	?CALLFI       -> 3;
	?CALLFII      -> 4;
	?CALLFIII     -> 5;
	?GESTALT      -> 3;
	?GETMEMSIZE   -> 1;
	?GLK          -> 3;
	?JEQ          -> 3;
	?JGE          -> 3;
	?JGEU         -> 3;
	?JGT          -> 3;
	?JLT          -> 3;
	?JNE          -> 3;
	?JNZ          -> 2;
	?JUMP         -> 1;
	?JZ           -> 2;
	?MUL          -> 3;
	?NOP          -> 0;
	?RETURN       -> 1;
	?SETIOSYS     -> 2;
	?SUB          -> 3;
	?STKCOPY      -> 1;
	?STREAMCHAR   -> 1;
	?STREAMNUM    -> 1;
	?STREAMSTR    -> 1;
	_Default      ->
	    io:format("unknown opcode at $~8.16.0B: #$~8.16.0B~n",
		      [Address, OpcodeNum])
    end,
    AddrModes = decode_operand_addr_modes(MachinePid, Address, NumOperands),
    AddrModesLength = (length(AddrModes) div 2) + (length(AddrModes) rem 2),
    %io:format("AddrModes: ~w~n", [AddrModes]),
    {AddrModesLength + operand_data_length(AddrModes),
     get_operands(MachinePid, Address + AddrModesLength, AddrModes)}.

-define(is_constant(AddrMode), AddrMode > 0, AddrMode =< 3).
-define(is_local(AddrMode), AddrMode >= 9, AddrMode =< 11).
-define(is_memory(AddrMode), AddrMode >= 5, AddrMode =< 7).
-define(is_ram(AddrMode), AddrMode >= 13, AddrMode =< 15).
-define(ADDRMODE_ZERO, 0).
-define(ADDRMODE_STACK_TOP, 8).

operand_data_length([]) -> 0;
operand_data_length([AddrMode | AddrModes]) ->
    if
	AddrMode =:= ?ADDRMODE_ZERO;
	AddrMode =:= ?ADDRMODE_STACK_TOP ->
	    operand_data_length(AddrModes);
	AddrMode =:= 1; AddrMode =:= 5;
	AddrMode =:= 9; AddrMode =:= 13  ->
	    1 + operand_data_length(AddrModes);
	AddrMode =:= 2; AddrMode =:= 6;
	AddrMode =:= 10; AddrMode =:= 14 ->
	    2 + operand_data_length(AddrModes);
	AddrMode =:= 3; AddrMode =:= 7;
	AddrMode =:= 11; AddrMode =:= 15 ->
	    4 + operand_data_length(AddrModes);
	true -> undef
    end.

get_operands(_MachinePid, _Address, []) -> [];
get_operands(MachinePid, Address, [?ADDRMODE_ZERO | AddrModes]) ->
    [ {const, 0} | get_operands(MachinePid, Address, AddrModes) ];
get_operands(MachinePid, Address, [?ADDRMODE_STACK_TOP | AddrModes]) ->
    [ {stack, undef} | get_operands(MachinePid, Address, AddrModes) ];
get_operands(MachinePid, Address, [AddrMode | AddrModes])
  when ?is_constant(AddrMode) ->
    case AddrMode of
	1 ->
	    [{const, ?get_signed_byte(Address)} |
	     get_operands(MachinePid, Address + 1, AddrModes)];
	2 ->
	    [{const, ?get_signed_word16(Address)} |
	     get_operands(MachinePid, Address + 2, AddrModes)];
	3 ->
	    [{const, ?get_word32(Address)} |
	     get_operands(MachinePid, Address + 4, AddrModes)];
	_Default ->
	    undef
    end;
get_operands(MachinePid, Address, [AddrMode | AddrModes])
  when ?is_memory(AddrMode) ->
    case AddrMode of
	5  ->
	    [{memory, ?get_byte(Address)} |
	     get_operands(MachinePid, Address + 1, AddrModes)];
	6 ->
	    [{memory, ?get_word16(Address)} |
	     get_operands(MachinePid, Address + 2, AddrModes)];
	7 ->
	    [{memory, ?get_word32(Address)} |
	     get_operands(MachinePid, Address + 4, AddrModes)];
	_Default ->
	    undef
    end;
get_operands(MachinePid, Address, [AddrMode | AddrModes])
  when ?is_local(AddrMode) ->
    case AddrMode of
	9  ->
	    [{local, ?get_byte(Address)} |
	     get_operands(MachinePid, Address + 1, AddrModes)];
	10 ->
	    [{local, ?get_word16(Address)} |
	     get_operands(MachinePid, Address + 2, AddrModes)];
	11 ->
	    [{local, ?get_word32(Address)} |
	     get_operands(MachinePid, Address + 4, AddrModes)];
	_Default ->
	    undef
    end;
get_operands(MachinePid, Address, [AddrMode | AddrModes])
  when ?is_ram(AddrMode) ->
    case AddrMode of
	13  ->
	    [{ram, ?get_byte(Address)} |
	     get_operands(MachinePid, Address + 1, AddrModes)];
	14 ->
	    [{ram, ?get_word16(Address)} |
	     get_operands(MachinePid, Address + 2, AddrModes)];
	15 ->
	    [{ram, ?get_word32(Address)} |
	     get_operands(MachinePid, Address + 4, AddrModes)];
	_Default ->
	    undef
    end.

%% decode operand modes
decode_operand_addr_modes(_MachinePid, _Address, NumOperands)
  when NumOperands =< 0 -> [];
decode_operand_addr_modes(MachinePid, Address, NumOperands) ->
    AddrModeByte = ?get_byte(Address),
    AddrMode1 = AddrModeByte band 2#00001111,
    AddrMode2 = (AddrModeByte bsr 4) band 2#00001111,
    case NumOperands of
	1        -> [AddrMode1];
	_Default -> [AddrMode1, AddrMode2] ++
			decode_operand_addr_modes(MachinePid, Address + 1,
						  NumOperands - 2)
    end.
