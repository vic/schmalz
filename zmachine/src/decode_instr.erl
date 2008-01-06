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
%%% Description of module decode_instr
%%%-----------------------------------------------------------------------
%%% This module contains the logic for decoding instructions
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%%
%%%-----------------------------------------------------------------------
-module(decode_instr).
-vsn('1.0').
-export([get_instruction/1]).
-include("include/zmachine.hrl").

-define(EXTENDED,           16#be).
-define(MASK_VAR,           2#11000000).
-define(MASK_SHORT,         2#10000000).

-define(MASK_UPPER_2_BITS,  2#11000000).
-define(MASK_LOWER_6_BITS,  2#00111111).
-define(MASK_LOWER_5_BITS,  2#00011111).
-define(MASK_LOWER_4_BITS,  2#00001111).
-define(MASK_BIT5,          2#00100000).
-define(MASK_BIT6,          2#01000000).
-define(MASK_BIT7,          2#10000000).
-define(MASK_BITS_4_5,      2#00110000).
-define(SHORT_OPCOUNT_0OP,  ?MASK_BITS_4_5).

-define(LEN_OPCODE,         1).
-define(LEN_LONG_OPERANDS,  2).
-define(LEN_STORE_VARIABLE, 1).

-define(variable_opcode_num(OpcodeByte), OpcodeByte band ?MASK_LOWER_5_BITS).
-define(long_opcode_num(OpcodeByte), OpcodeByte band ?MASK_LOWER_5_BITS).
-define(short_opcode_num(OpcodeByte), OpcodeByte band ?MASK_LOWER_4_BITS).
-define(DUMMY_BRANCH_INFO, instruction:create_branch_info(undef, 0, 0, undef)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Top level
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% since we are using a functional language, instructions should be
% implemented as function objects
% get_instruction should return a function object in combination with
% the required parameters

get_instruction(ServerRef) ->
    Address = ?call_machine(pc),
    Form = opcode_type(?get_byte(Address)),
    Version = ?call_machine(version),
    case Form of
	short    -> decode_short(ServerRef, Address, Version);
	long     -> decode_long(ServerRef, Address, Version);
	variable -> decode_variable(ServerRef, Address, Version);
	extended -> decode_extended(ServerRef, Address, Version)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Short instructions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_short(ServerRef, Address, Version) ->
    OpcodeByte = ?get_byte(Address),
    OperandCount = short_operand_count(OpcodeByte),
    OpcodeNum = ?short_opcode_num(OpcodeByte),
    % retrieve the type info at position 1, there is only one
    OperandType = get_op_type(OpcodeByte, 1),
    IsPrint = is_print(OperandCount, OpcodeNum),
    {Params, ZsciiLength} = if
	IsPrint ->
	    Str = ?call_machine({decode_address, Address + 1, undef}),
	    {[{zscii, Str}], ?call_machine({num_zencoded_bytes, Address + 1})};
        true ->
	    {[{OperandType, get_operand_at(ServerRef, Address + 1,
					   OperandType)}], 0}
    end,
    NumOperandBytes = operand_length(OperandType),
    StoreVarAddress = Address + ?LEN_OPCODE + NumOperandBytes,
    StoreVar =
	storevar(ServerRef, StoreVarAddress,
		 instruction_info:is_store(OperandCount, OpcodeNum, Version)),
    BranchInfoAddress = StoreVarAddress + storevar_len(StoreVar),
    BranchInfo = branch_info(ServerRef, BranchInfoAddress,
			     instruction_info:is_branch(OperandCount,
							OpcodeNum,
							undef)),
    OpcodeLength = ?LEN_OPCODE + NumOperandBytes +
	storevar_len(StoreVar) + instruction:size_branch_offset(BranchInfo) +
	ZsciiLength,
    instruction:create(OperandCount, OpcodeNum, Params, StoreVar,
		       BranchInfo, Address, OpcodeLength).

is_print(oc_0op, OpcodeNum)
  when OpcodeNum =:= ?PRINT; OpcodeNum =:= ?PRINT_RET -> true;
is_print(_OperandCount, _OpcodeNum)                   -> false.
    
%% retrieves the operand count encoded in the specified short opcode
%% @spec short_operand_count(int()) -> atom()
short_operand_count(OpcodeByte) ->
    case OpcodeByte band ?MASK_BITS_4_5 of
	?SHORT_OPCOUNT_0OP -> oc_0op;
	_Default -> oc_1op
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Variable instructions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% extracts the properties of the specified variable instruction
decode_variable(ServerRef, Address, Version) ->
    OpcodeByte = ?get_byte(Address),
    OperandCount = variable_operand_count(OpcodeByte),
    OpcodeNum = ?variable_opcode_num(OpcodeByte),
    if
	OperandCount =:= oc_var, OpcodeNum =:= ?CALL_VS2 ->
	    OperandTypes = extract_operand_types(?get_byte(Address + 1)) ++
		extract_operand_types(?get_byte(Address + 2)),
	    OpTypesOffset = 3;
	true                    ->
	    OperandTypes = extract_operand_types(?get_byte(Address + 1)),
	    OpTypesOffset = 2
    end,
    create_var_instruction(ServerRef, Address, Version, OperandCount,
			   OpcodeNum, OperandTypes, OpTypesOffset).
	    
create_var_instruction(ServerRef, Address, Version, OperandCount, OpcodeNum,
		       OperandTypes, OpTypesOffset) ->
    Operands = extract_operands(ServerRef, Address + OpTypesOffset,
				OperandTypes),
    StoreVarAddress = Address + OpTypesOffset + num_operand_bytes(OperandTypes),
    StoreVar = storevar(ServerRef, StoreVarAddress,
			instruction_info:is_store(OperandCount, OpcodeNum,
						  Version)),
    BranchInfoAddress = StoreVarAddress + storevar_len(StoreVar),
    BranchInfo = branch_info(ServerRef, BranchInfoAddress,
			     instruction_info:is_branch(OperandCount,
							OpcodeNum,
							undef)),
    OpcodeLength = OpTypesOffset + num_operand_bytes(OperandTypes) +
	storevar_len(StoreVar) + instruction:size_branch_offset(BranchInfo),
    instruction:create(OperandCount, OpcodeNum, 
		       lists:zip(OperandTypes, Operands),
		       StoreVar, BranchInfo, Address,
		       OpcodeLength).

% extracts the the operand count from the specified opcode
variable_operand_count(OpcodeByte) ->
    if 
	(OpcodeByte band ?MASK_BIT5) =:= 0 -> oc_2op;
	true -> oc_var
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Long instructions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% decodes the long instruction at the specified address
decode_long(ServerRef, Address, Version) ->
    Opcode = ?get_byte(Address),
    OpcodeNum = ?long_opcode_num(Opcode),
    StoreVarAddress = Address + ?LEN_OPCODE + ?LEN_LONG_OPERANDS,
    StoreVar = storevar(ServerRef, StoreVarAddress,
			instruction_info:is_store(oc_2op, OpcodeNum, Version)),
    BranchInfoAddress = StoreVarAddress + storevar_len(StoreVar), 
    BranchInfo = branch_info(ServerRef, BranchInfoAddress,
			     instruction_info:is_branch(oc_2op, OpcodeNum,
							Version)),
    OpcodeLength = ?LEN_OPCODE + ?LEN_LONG_OPERANDS + storevar_len(StoreVar) +
	instruction:size_branch_offset(BranchInfo),
    Operands = [ {long_operand_type1(Opcode), ?get_byte(Address + 1)},
		 {long_operand_type2(Opcode), ?get_byte(Address + 2)} ],
    instruction:create(oc_2op, OpcodeNum, Operands, StoreVar,
		       BranchInfo, Address, OpcodeLength).

% determine the type of operand 1 in the specified long opcode
long_operand_type1(Opcode) when Opcode band ?MASK_BIT6 /= 0 -> variable;
long_operand_type1(_Opcode) -> small_constant.

% determine the type of operand 2 in the specified long opcode
long_operand_type2(Opcode) when Opcode band ?MASK_BIT5 /= 0 -> variable;
long_operand_type2(_Opcode) -> small_constant.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Extended instructions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% extracts the properties of the specified variable instruction
decode_extended(ServerRef, Address, Version) ->
    OpcodeNum = ?get_byte(Address + 1),
    OperandTypes = extract_operand_types(?get_byte(Address + 2)),
    OpTypesOffset = 3,
    create_var_instruction(ServerRef, Address, Version, oc_ext,
			   OpcodeNum, OperandTypes, OpTypesOffset).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% General purpose helpers for instructions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% retrieves the store variable at the specified address if possible, otherwise
% undef will be returned
storevar(ServerRef, StoreVarAddress, true)       -> ?get_byte(StoreVarAddress);
storevar(_ServerRef, _StoreVarAddress, _IsStore) -> undef.

% retrieves the length of the store variable. If not set, the length is 0,
% otherwise it is 1
storevar_len(undef)     -> 0;
storevar_len(_StoreVar) -> 1.

% extracts the branch information stored at the specified address
branch_info(ServerRef, BranchInfoAddress, true) -> 
    BranchByte1 = ?get_byte(BranchInfoAddress),
    BranchOnTrue = BranchByte1 band ?MASK_BIT7 /= 0,
    IsShortOffset = BranchByte1 band ?MASK_BIT6 /= 0,
    {NumOffsetBytes, BranchOffset} =
	if
	    IsShortOffset -> {1, BranchByte1 band ?MASK_LOWER_6_BITS};
	    true ->
		BranchByte2 = ?get_byte(BranchInfoAddress + 1),
		LongBranchOffset = util:unsigned_to_signed14(
				     (((BranchByte1 band ?MASK_LOWER_6_BITS)
				       bsl 8) bor BranchByte2)),
		{ 2, LongBranchOffset }
	end,
    instruction:create_branch_info(BranchOnTrue, NumOffsetBytes,
				   BranchInfoAddress + NumOffsetBytes,
				   BranchOffset);
branch_info(_ServerRef, _BranchInfoAddress, _IsBranch) ->
    ?DUMMY_BRANCH_INFO.

% determines the opcode type from the first opcode byte
opcode_type(OpcodeByte1) ->
    if
	OpcodeByte1                    =:= ?EXTENDED   -> extended;
	(OpcodeByte1 band ?MASK_VAR)   =:= ?MASK_VAR   -> variable;
	(OpcodeByte1 band ?MASK_SHORT) =:= ?MASK_SHORT -> short;
	true -> long
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Operand extraction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% extracts the list of operand types from the given operand type byte
extract_operand_types(OpTypeByte) ->
    lists:reverse(extract_operand_types(OpTypeByte, 0, [])).

% From the operand type byte, extract the operand types, stop, if an ommitted
% is encountered
extract_operand_types(_OpTypeByte, Pos, Types) when Pos =:= 4 -> Types;
extract_operand_types(OpTypeByte, Pos, Types) ->
    Type = get_op_type(OpTypeByte, Pos),
    case Type of
	omitted -> Types;
	_Default -> extract_operand_types(OpTypeByte, Pos + 1, [ Type | Types ])
    end.

% determines the operand type at the specified position within the byte,
% there are 4 (0..3) 2-bit positions in each byte starting from the left
get_op_type(OpTypeByte, Pos) ->
    Bits = (OpTypeByte bsr (6 - Pos * 2)) band 2#00000011,
    case Bits of
	2#00 -> large_constant;
	2#01 -> small_constant;
	2#10 -> variable;
	2#11 -> omitted
    end.

% extracts the operands at the specified position with the specified
% operand types
extract_operands(ServerRef, Address, OperandTypes) ->
    lists:reverse(extract_operands(ServerRef, Address, OperandTypes, [])).
extract_operands(_ServerRef, _Address, [], Output) -> Output;
extract_operands(ServerRef, Address, [ OperandType | OperandTypes ],
		 Output) ->
    if
	OperandType =:= large_constant                           ->
	    Constant = ?get_word16(Address),
	    extract_operands(ServerRef, Address + 2, OperandTypes,
			     [ Constant | Output ]);
	OperandType =:= small_constant; OperandType =:= variable ->
	    Operand = ?get_byte(Address),
	    extract_operands(ServerRef, Address + 1, OperandTypes,
			     [ Operand | Output ])
    end.

% determines the number of bytes that the operands will use up,
% given a list of operand types
num_operand_bytes(OperandTypes) ->
    lists:sum(lists:map(fun operand_length/1, OperandTypes)).

% maps an operand type to a number of bytes
operand_length(large_constant) -> 2;
operand_length(omitted)        -> 0;
operand_length(zscii)          -> 0;
operand_length(_OperandType)   -> 1.

% retrieves an operand at the specified address
get_operand_at(ServerRef, Address, large_constant) -> ?get_word16(Address);
get_operand_at(ServerRef, Address, _OperandCount)  -> ?get_byte(Address).
