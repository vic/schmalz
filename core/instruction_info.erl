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
%%% Description of module instruction_info
%%%-----------------------------------------------------------------------
%%% This module supports decode_instr and instruction and contains
%%% functions used in both in order to operate
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%%
%%%-----------------------------------------------------------------------
-module(instruction_info).
-export([is_branch/3, is_store/3, is_call/3, is_return/3, is_jump/3]).
-include("include/opcodes.hrl").

% determines whether the specified opcode is a branch opcode
is_branch(oc_0op, OpcodeNum, _Version)
  when OpcodeNum =:= ?VERIFY; OpcodeNum =:= ?PIRACY              -> true;
is_branch(oc_1op, OpcodeNum, _Version)
  when OpcodeNum =:= ?JZ; OpcodeNum =:= ?GET_CHILD;
       OpcodeNum =:= ?GET_SIBLING                                -> true;
is_branch(oc_2op, OpcodeNum, _Version)
  when OpcodeNum =:= ?JE; OpcodeNum =:= ?JL; OpcodeNum =:= ?JG;
       OpcodeNum =:= ?DEC_CHK; OpcodeNum =:= ?INC_CHK; OpcodeNum =:= ?JIN;
       OpcodeNum =:= ?TEST; OpcodeNum =:= ?TEST_ATTR             -> true;
is_branch(variable, OpcodeNum, _Version)
  when OpcodeNum =:= ?SCAN_TABLE; OpcodeNum =:= ?CHECK_ARG_COUNT -> true;
is_branch(_OpCount, _OpcodeNum, _Version)                        -> false.

% determines whether the specified opcode is a store opcode
is_store(oc_1op, OpcodeNum, _Version)
  when OpcodeNum =:= ?GET_PARENT; OpcodeNum =:= ?GET_CHILD;
       OpcodeNum =:= ?GET_SIBLING; OpcodeNum =:= ?GET_PROP_LEN;
       OpcodeNum =:= ?CALL_1S; OpcodeNum =:= ?LOAD;
       OpcodeNum =:= ?NOT-> true;
is_store(oc_2op, OpcodeNum, _Version)
  when OpcodeNum =:= ?OR; OpcodeNum =:= ?AND; OpcodeNum =:= ?LOADW;
       OpcodeNum =:= ?LOADB; OpcodeNum =:= ?GET_PROP;
       OpcodeNum =:= ?GET_PROP_ADDR;
       OpcodeNum =:= ?GET_NEXT_PROP; OpcodeNum =:= ?ADD; OpcodeNum =:= ?SUB;
       OpcodeNum =:= ?MUL; OpcodeNum =:= ?DIV; OpcodeNum =:= ?MOD;
       OpcodeNum =:= ?CALL_2S             -> true;
is_store(oc_var, OpcodeNum, Version)
  when Version >= 5, OpcodeNum =:= ?AREAD -> true;
is_store(oc_var, OpcodeNum, _Version)
  when OpcodeNum =:= ?CALL; OpcodeNum =:= ?RANDOM; OpcodeNum =:= ?CALL_VS2;
       OpcodeNum =:= ?READ_CHAR; OpcodeNum =:= ?SCAN_TABLE;
       OpcodeNum =:= ?NOT_V5             -> true;
is_store(_OpCount, _OpcodeNum, _Version) -> false.

% determines whether the specified opcode is a call
is_call(oc_var, OpcodeNum, _Version) when OpcodeNum =:= ?CALL    -> true;
is_call(oc_2op, OpcodeNum, _Version) when OpcodeNum =:= ?CALL_2S -> true;
is_call(_OpCount, _OpcodeNum, _Version)                          -> false.

is_return(oc_1op, ?RET, _Version)                           -> true;
is_return(oc_0op, OpcodeNum, _Version)
  when OpcodeNum =:= ?RTRUE; OpcodeNum =:= ?RFALSE;
       OpcodeNum =:= ?RET_POPPED; OpcodeNum =:= ?PRINT_RET  -> true;
is_return(_OpCount, _OpcodeNum, _Version)                   -> false.

is_jump(oc_1op, ?JUMP, _Version)        -> true;
is_jump(_OpCount, _OpcodeNum, _Version) -> false.
