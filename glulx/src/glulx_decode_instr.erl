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
    case First band 2#11000000 of
	2#11000000 ->
	    OpNumLen = 4,
	    OpcodeNum = ?get_word32(Address) - 16#c0000000;
	2#10000000 ->
	    OpNumLen = 2,
	    OpcodeNum = ?get_word16(Address) - 16#8000;
	2#00000000 ->
	    OpNumLen = 1,
	    OpcodeNum = First
    end,
    {OperandDataSize, Operands} = decode_operands(MachinePid,
						  Address + OpNumLen,
						  OpcodeNum),
    #instr{opcode = OpcodeNum, operands = Operands, address = Address,
	   length = OpNumLen + OperandDataSize}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Helper fuctionality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_operands(MachinePid, Address, OpcodeNum) ->
    NumOperands = case OpcodeNum of
	?CALL -> 3;
	_Default ->
	    io:format("unknown opcode: ~w~n", [OpcodeNum])
    end,
    AddrModes = decode_operand_addr_modes(MachinePid, Address, NumOperands),
    AddrModesLength = (length(AddrModes) div 2) + (length(AddrModes) rem 2),
    {AddrModesLength + operand_data_length(AddrModes),
     get_operands(MachinePid, Address + AddrModesLength, AddrModes)}.

-define(is_constant(AddrMode), AddrMode > 0, AddrMode =< 3).
-define(ADDRMODE_ZERO, 0).
-define(ADDRMODE_STACK_TOP, 8).

operand_data_length([]) -> 0;
operand_data_length([AddrMode | AddrModes]) ->
    if 
	AddrMode =:= ?ADDRMODE_ZERO; AddrMode =:= ?ADDRMODE_STACK_TOP ->
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
    [ {stack_top, undef} | get_operands(MachinePid, Address, AddrModes) ];
get_operands(MachinePid, Address, [AddrMode | AddrModes])
  when ?is_constant(AddrMode) ->
    case AddrMode of
	1 ->
	    [{const, ?get_byte(Address)} |
	     get_operands(MachinePid, Address + 1, AddrModes)];
	2 ->
	    [{const, ?get_word16(Address)} |
	     get_operands(MachinePid, Address + 2, AddrModes)];
	3 ->
	    [{const, ?get_word32(Address)} |
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
