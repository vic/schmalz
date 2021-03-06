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
%%% Description of module instruction
%%%-----------------------------------------------------------------------
%%% This module implements the abstract data types Instruction and
%%% BranchInfo.
%%% It was decided to separate decoding from execution to have a
%%% cleaner design. All operations of the Z-Machine are implemented in
%%% this module, so it is pretty large, by putting them all in one module
%%% we avoid exporting a huge amount of functions, effectively minimizing
%%% the public interface.
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%% create(Form, OperandCount, OpcodeNum, Operands, StoreVariable,
%%%        BranchInfo, Address, OpcodeLength)
%%%   creates an Instruction
%%%
%%% create_branch_info(BranchOnTrue, NumOffsetBytes, Offset)
%%%   creates a BranchInfo
%%%
%%% size_branch_offset(BranchInfo)
%%%   number of bytes the BranchInfo occupies in the instruction
%%%
%%% execute(Instruction, ServerRef)
%%%   executes an Instruction that might have an effect on MachineState
%%%
%%% print(Instruction)
%%%   prints a representation of the Instruction
%%%-----------------------------------------------------------------------

-module(instruction).
-export([create/7, create_branch_info/4, size_branch_offset/1,
	 execute/2, print_instr/3]).
-include("include/zmachine.hrl").

%% Useful Macros
-define(FALSE, 0).
-define(TRUE,  1).
-define(STACK, 0).
-define(trunc16(Value), Value band 16#ffff).
-define(USE_UNSIGNED_PARAMETERS, Parameters = params(ServerRef, Operands)).
-define(USE_SIGNED_PARAMETERS,
	Parameters = signed_params(ServerRef, Operands)).
-define(param(Index), lists:nth(Index, Parameters)).
-define(store_var2(StoreVar, Value),
	?call_machine({set_var, StoreVar, util:signed_to_unsigned16(Value)})).
-define(store_var(Value), ?store_var2(StoreVar, Value)).
-define(return_from_routine(ReturnValue),
	?call_machine({return_from_routine, ReturnValue})).
-define(next_instruction(),
	?call_machine({increment_pc, OpcodeLength})).

-define(branch_or_advance(Condition),
	branch_or_advance(ServerRef, Instruction, Condition)).

%%%-----------------------------------------------------------------------
%%% BranchInfo representation
%%%
%%% The BranchInfo type is tightly related to the Instruction object,
%%% so it is defined in the same module. Through this means we also
%%% hide the implementation within the instruction module
%%%-----------------------------------------------------------------------

-record(branch_info, {branch_on_true, num_offset_bytes,
		      address_after_branch_data, offset}).

create_branch_info(BranchOnTrue, NumOffsetBytes, AddressAfterBranchData,
		   Offset) ->
    #branch_info{branch_on_true = BranchOnTrue,
		 num_offset_bytes = NumOffsetBytes,
		 address_after_branch_data = AddressAfterBranchData,
		 offset = Offset}.

size_branch_offset(#branch_info{num_offset_bytes = NumOffsetBytes}) ->
    NumOffsetBytes.

%%%-----------------------------------------------------------------------
%%% Instruction representation
%%%-----------------------------------------------------------------------

-record(instruction, {operand_count, opcode_num, operands,
		      store_variable, branch_info, address, opcode_length}).

%% creates a new Instruction instance
create(OperandCount, OpcodeNum, Operands, StoreVariable,
       BranchInfo, Address, OpcodeLength) ->
    #instruction{operand_count = OperandCount,
		 opcode_num = OpcodeNum, operands = Operands,
		 store_variable = StoreVariable, branch_info = BranchInfo,
		 address = Address, opcode_length = OpcodeLength}.

execute(Instruction, ServerRef) ->
    Version = ?call_machine(version),
    Execute = get_operation(Instruction, Version),
    Execute(Instruction, ServerRef),
    update_pc(Instruction, ServerRef, Version).

update_pc(#instruction{operand_count = OperandCount, opcode_num = OpcodeNum,
		       opcode_length = OpcodeLength}, ServerRef, Version) ->
    IsCall   = instruction_info:is_call(OperandCount, OpcodeNum, Version),
    IsBranch = instruction_info:is_branch(OperandCount, OpcodeNum, Version),
    IsReturn = instruction_info:is_return(OperandCount, OpcodeNum, Version),
    IsJump   = instruction_info:is_jump(OperandCount, OpcodeNum, Version),
    Status = ?call_machine(status),
    IsReading = (Status =:= sread) or (Status =:= read_char),
    if
	IsCall; IsBranch; IsReturn; IsJump;
	IsReading -> keep_pc;
	true      -> ?next_instruction()
    end.
  
get_operation(#instruction{operand_count = oc_0op, opcode_num = OpcodeNum },
	      _Version) ->
    case OpcodeNum of
	?NEW_LINE         -> fun new_line/2;
	?NOP              -> fun nop/2;
	?PIRACY           -> fun piracy/2;
	?POP              -> fun pop/2;
	?PRINT            -> fun print/2;
	?PRINT_RET        -> fun print_ret/2;
	?QUIT             -> fun op_halt/2;
	?RET_POPPED       -> fun ret_popped/2;
	?RTRUE            -> fun rtrue/2;
	?RFALSE           -> fun rfalse/2;
	?SHOW_STATUS      -> fun show_status/2;
	?VERIFY           -> fun verify/2;
	_Default          -> fun op_halt/2
    end;
get_operation(#instruction{operand_count = oc_1op, opcode_num = ?CALL_1N},
	      Version) when Version >= 5 -> fun call_1s/2;
get_operation(#instruction{operand_count = oc_1op, opcode_num = OpcodeNum},
	      _Version) ->
    case OpcodeNum of
	?CALL_1S          -> fun call_1s/2;
	?DEC              -> fun dec/2;
	?GET_CHILD        -> fun get_child/2;
	?GET_PARENT       -> fun get_parent/2;
	?GET_PROP_LEN     -> fun get_prop_len/2;
	?GET_SIBLING      -> fun get_sibling/2;
	?INC              -> fun inc/2;
	?JUMP             -> fun jump/2;
	?JZ               -> fun jz/2;
	?LOAD             -> fun load/2;
	?NOT              -> fun op_not/2;
	?PRINT_ADDR       -> fun print_addr/2;
	?PRINT_OBJ        -> fun print_obj/2;
	?PRINT_PADDR      -> fun print_paddr/2;
	?REMOVE_OBJ       -> fun remove_obj/2;
	?RET              -> fun ret/2;
	_Default          -> fun op_halt/2
    end;
get_operation(#instruction{operand_count = oc_2op, opcode_num = ?CALL_2N},
	      Version) when Version >= 5 -> fun call_2s/2;
get_operation(#instruction{operand_count = oc_2op, opcode_num = OpcodeNum},
	      _Version) ->
    case OpcodeNum of
	?ADD              -> fun add/2;
	?AND              -> fun op_and/2;
	?CALL_2S          -> fun call_2s/2;
	?CLEAR_ATTR       -> fun clear_attr/2;
	?DEC_CHK          -> fun dec_chk/2;
	?DIV              -> fun op_div/2;
	?GET_NEXT_PROP    -> fun get_next_prop/2;
	?GET_PROP         -> fun get_prop/2;
	?GET_PROP_ADDR    -> fun get_prop_addr/2;
	?INC_CHK          -> fun inc_chk/2;
	?INSERT_OBJ       -> fun insert_obj/2;
	?JE               -> fun je/2;
	?JG               -> fun jg/2;
	?JIN              -> fun jin/2;
	?JL               -> fun jl/2;
	?LOADB            -> fun loadb/2;
	?LOADW            -> fun loadw/2;
	?MOD              -> fun mod/2;
	?MUL              -> fun mul/2;
	?OR               -> fun op_or/2;
	?SET_ATTR         -> fun set_attr/2;
	?SUB              -> fun sub/2;
	?STORE            -> fun store/2;
	?TEST             -> fun test/2;
	?TEST_ATTR        -> fun test_attr/2;
	_Default          -> fun op_halt/2
    end;
get_operation(#instruction{operand_count = oc_var, opcode_num = ?CALL_VN},
	      Version) when Version >= 5 -> fun call/2;
get_operation(#instruction{operand_count = oc_var, opcode_num = OpcodeNum },
	      _Version) ->
    case OpcodeNum of
	?BUFFER_MODE      -> fun buffer_mode/2;
	?CALL             -> fun call/2;
	?CALL_VS2         -> fun call_vs2/2;
	?ERASE_WINDOW     -> fun erase_window/2;
	?OUTPUT_STREAM    -> fun output_stream/2;
	?PRINT_NUM        -> fun print_num/2;
	?PRINT_CHAR       -> fun print_char/2;
	?PUSH             -> fun push/2;
	?PULL             -> fun pull/2;
	?PUT_PROP         -> fun put_prop/2;
	?RANDOM           -> fun random/2;
	?READ_CHAR        -> fun read_char/2;
	?SCAN_TABLE       -> fun scan_table/2;
	?SET_CURSOR       -> fun set_cursor/2;
	?SET_TEXT_STYLE   -> fun set_text_style/2;
	?SET_WINDOW       -> fun set_window/2;
	?SOUND_EFFECT     -> fun sound_effect/2;
	?SPLIT_WINDOW     -> fun split_window/2;
	?SREAD            -> fun sread/2;
	?STOREB           -> fun storeb/2;
	?STOREW           -> fun storew/2;
	_Default          -> fun op_halt/2
    end;
get_operation(#instruction{operand_count = oc_ext, opcode_num = OpcodeNum },
	      _Version) ->
    case OpcodeNum of
	?SAVE_UNDO        -> fun save_undo/2;
	_Default          -> fun op_halt/2
    end.

%%%-----------------------------------------------------------------------
%%% The operations
%%%-----------------------------------------------------------------------

add(#instruction{operands = Operands, store_variable = StoreVar},
    ServerRef) ->
    ?USE_SIGNED_PARAMETERS,
    % the strange thing is that if we do not go explicitly, the
    % value sent to the VM is plain wrong under OTP 5.5.5 and 5.6.
    % This could be caused by the macro expansion or the send mechanism.
    % the workaround is to calculate first and send the calculated value.
    Sum = ?param(1) + ?param(2),
    ?store_var(?trunc16(Sum)).

op_and(#instruction{operands = Operands, store_variable = StoreVar},
    ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    % see @add
    Result = ?param(1) band ?param(2),
    ?store_var(Result).

buffer_mode(_, _) -> void. % not implemented

% call is also call_vn, see call_1s
call(#instruction{operands = Operands} = Instruction, ServerRef) ->
    [ PackedAddress | Arguments] = params(ServerRef, Operands),
    call_with_result(Instruction, PackedAddress, Arguments, ServerRef).

% call_1s is also call_1n, because there is no store specified, it will
% be thrown away
call_1s(#instruction{operands = Operands} = Instruction, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    call_with_result(Instruction, ?param(1), [], ServerRef).

% call_2s is also call_2n, see call_1s
call_2s(#instruction{operands = Operands} = Instruction, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    call_with_result(Instruction, ?param(1), [?param(2)], ServerRef).

call_vs2(#instruction{operands = Operands} = Instruction, ServerRef) ->
    [Routine | Params] = params(ServerRef, Operands),
    %io:format("@call_vs, routine: ~w, Params: ~p~n", [Routine, Params]),
    call_with_result(Instruction, Routine, Params, ServerRef).

call_with_result(#instruction{address = Address, opcode_length = OpcodeLength,
			      store_variable = StoreVar},
		 PackedAddress, Arguments, ServerRef) ->
    if
	PackedAddress =:= 0 ->
	    ?store_var(?FALSE),
	    ?next_instruction();
	true                ->
	    ?call_machine({call_routine, PackedAddress, Address + OpcodeLength,
			   Arguments, StoreVar})
    end.

clear_attr(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?call_machine({object_clear_attribute, ?param(1), ?param(2)}).

dec(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    VarNum = ?param(1),
    Value = ?call_machine({get_var, VarNum}),
    % see @add
    Result = util:unsigned_to_signed16(Value) - 1,
    ?store_var2(VarNum, ?trunc16(Result)).

dec_chk(#instruction{operands = Operands} = Instruction, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    VarNum = ?param(1),
    Value = ?call_machine({get_var, VarNum}),
    DecValue = util:unsigned_to_signed16(Value) - 1,
    ?call_machine({set_var, VarNum, DecValue}),
    ?branch_or_advance(DecValue < ?param(2)).

op_div(#instruction{operands = Operands, store_variable = StoreVar},
       ServerRef) ->
    ?USE_SIGNED_PARAMETERS,
    Quotient = ?param(1) div ?param(2),
    ?store_var(?trunc16(Quotient)).

erase_window(#instruction{operands = Operands}, ServerRef) ->
    ?USE_SIGNED_PARAMETERS,
    ?call_machine({erase_window, ?param(1)}).

get_child(#instruction{operands = Operands, store_variable = StoreVar}
	  = Instruction, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    FirstChild = ?call_machine({object_child, ?param(1)}),
    ?store_var(FirstChild),
    ?branch_or_advance(FirstChild /= 0).

get_parent(#instruction{operands = Operands, store_variable = StoreVar},
	   ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?store_var(?call_machine({object_parent, ?param(1)})).

get_sibling(#instruction{operands = Operands, store_variable = StoreVar}
	    = Instruction, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    Sibling = ?call_machine({object_sibling, ?param(1)}),
    ?store_var(Sibling),
    ?branch_or_advance(Sibling /= 0).

get_next_prop(#instruction{operands = Operands, store_variable = StoreVar},
	      ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?store_var(?call_machine({object_next_property, ?param(1), ?param(2)})).

get_prop(#instruction{operands = Operands, store_variable = StoreVar},
	      ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?store_var(?call_machine({object_property, ?param(1), ?param(2)})).

get_prop_addr(#instruction{operands = Operands, store_variable = StoreVar},
	      ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?store_var(?call_machine({object_prop_addr, ?param(1), ?param(2)})).

get_prop_len(#instruction{operands = Operands, store_variable = StoreVar},
	      ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?store_var(?call_machine({object_prop_len, ?param(1)})).

inc(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    VarNum = ?param(1),
    Value = ?call_machine({get_var, VarNum}),
    % see @add
    Result = util:unsigned_to_signed16(Value) + 1,
    ?store_var2(VarNum, ?trunc16(Result)).

inc_chk(#instruction{operands = Operands} = Instruction, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    VarNum = ?param(1),
    Value = ?call_machine({get_var, VarNum}),
    IncValue = util:unsigned_to_signed16(Value) + 1,
    ?call_machine({set_var, VarNum, IncValue}),
    ?branch_or_advance(IncValue > ?param(2)).

insert_obj(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?call_machine({insert_object, ?param(1), ?param(2)}).

je(#instruction{operands = Operands} = Instruction, ServerRef) ->
    [First | CompareList] = signed_params(ServerRef, Operands),
    %io:format("@JE, First: ~w, CompareList: ~p~n", [First, CompareList]),
    ?branch_or_advance(
       length(lists:filter(fun(X) -> X =:= First end, CompareList)) > 0).

jg(#instruction{operands = Operands} = Instruction, ServerRef) ->
    ?USE_SIGNED_PARAMETERS,
    ?branch_or_advance(?param(1) > ?param(2)).

jin(#instruction{operands = Operands} = Instruction, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?branch_or_advance(?call_machine({object_parent, ?param(1)}) =:= ?param(2)).

jl(#instruction{operands = Operands} = Instruction, ServerRef) ->
    ?USE_SIGNED_PARAMETERS,
    ?branch_or_advance(?param(1) < ?param(2)).


jump(#instruction{operands = Operands}, ServerRef) ->
    ?USE_SIGNED_PARAMETERS,
    ?call_machine({set_pc, ?call_machine(pc) + ?param(1) + 1}).

jz(#instruction{operands = Operands} = Instruction, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?branch_or_advance(?param(1) =:= 0).

load(#instruction{operands = Operands, store_variable = StoreVar},
     ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?store_var(?call_machine({get_var, ?param(1)})).

loadb(#instruction{operands = Operands, store_variable = StoreVar},
      ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?store_var(?call_machine({get_byte, ?param(1) + ?param(2)})).

loadw(#instruction{operands = Operands, store_variable = StoreVar},
      ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?store_var(?call_machine({get_word16, ?param(1) + 2 * ?param(2)})).

mod(#instruction{operands = Operands, store_variable = StoreVar},
    ServerRef) ->
    ?USE_SIGNED_PARAMETERS,
    % see @add
    Remainder = ?param(1) rem ?param(2),
    ?store_var(?trunc16(Remainder)).

mul(#instruction{operands = Operands, store_variable = StoreVar},
    ServerRef) ->
    ?USE_SIGNED_PARAMETERS,
    % see @add
    Product = ?param(1) * ?param(2),
    ?store_var(?trunc16(Product)).

new_line(_Instruction, ServerRef) ->
    ?call_machine({print_zscii, [?NEWLINE]}).

nop(_Instruction, _ServerRef) -> void.

op_not(#instruction{operands = Operands, store_variable = StoreVar},
       ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    % see @add
    Result = bnot ?param(1),
    ?store_var(Result).

op_or(#instruction{operands = Operands, store_variable = StoreVar},
    ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    % see @add
    Result = ?param(1) bor ?param(2),
    ?store_var(Result).

output_stream(#instruction{operands = Operands}, ServerRef) ->
    ?USE_SIGNED_PARAMETERS,
    case length(Operands) of
	1 -> ?call_machine({output_stream, ?param(1)});
	2 -> ?call_machine({output_stream, ?param(1), ?param(2)})
    end.

piracy(Instruction, ServerRef) -> ?branch_or_advance(true).

pop(_Instruction, ServerRef) ->
    ?call_machine({get_var, ?STACK}).

print(#instruction{operands = [{zscii, String}]}, ServerRef) ->
    ?call_machine({print_zscii, String}).

print_addr(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?call_machine({print_addr, ?param(1)}).

print_ret(#instruction{operands = [{zscii, String}]}, ServerRef) ->
    ?call_machine({print_zscii, String}),
    ?call_machine({print_zscii, [?NEWLINE]}),
    ?return_from_routine(?TRUE).    

print_char(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?call_machine({print_zscii, [?param(1)]}).

print_num(#instruction{operands = Operands}, ServerRef) ->
    ?USE_SIGNED_PARAMETERS,
    ?call_machine({print_zscii, io_lib:write(?param(1))}).

print_obj(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?call_machine({print_object, ?param(1)}).

print_paddr(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?call_machine({print_paddr, ?param(1)}).

pull(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    Value = ?call_machine({get_var, ?STACK}),
    ?store_var2(?param(1), Value). 

push(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?store_var2(?STACK, ?param(1)). 

put_prop(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?call_machine({object_set_property, ?param(1), ?param(2), ?param(3)}).

random(#instruction{operands = Operands, store_variable = StoreVar},
       ServerRef) ->
    ?USE_SIGNED_PARAMETERS,
    Range = ?param(1),
    if
	Range =:= 0 ->
	    random:seed0(),
	    ?store_var(0);
	Range < 0   ->
	    random:seed(-Range, 0, 0),
	    ?store_var(0);
	true ->
	    ?store_var(random:uniform(?param(1)))
    end.

read_char(#instruction{store_variable = StoreVar}, ServerRef) ->
    MachineStatus = ?call_machine(status),
    if
	MachineStatus =:= run ->
	    % if this is the first invocation of the READ instruction,
	    % change machine status to sread
            ?call_machine({set_status, read_char});
        true ->
	    % this is the second invocation, parse the input
	    % and write the results into the buffers
	    ?call_machine({read_char, StoreVar}),
            ?call_machine({set_status, run})
    end.

remove_obj(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?call_machine({remove_object, ?param(1)}).

ret(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?return_from_routine(?param(1)).

ret_popped(_Instruction, ServerRef) ->
    StackValue = ?call_machine({get_var, ?STACK}),
    ?return_from_routine(StackValue).

rfalse(_Instruction, ServerRef) ->
    ?return_from_routine(?FALSE).

rtrue(_Instruction, ServerRef) ->
    ?return_from_routine(?TRUE).

save_undo(#instruction{store_variable = StoreVar}, ServerRef) ->
    % Undo not implemented
    ?store_var(-1).

scan_table(#instruction{operands = Operands, store_variable = StoreVar}
	   = Instruction, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    NumParams = length(Operands),
    if
	NumParams =:= 3 ->
	    Addr = ?call_machine({scan_table, ?param(1), ?param(2), ?param(3)});
	true            ->
	    Addr = ?call_machine({scan_table, ?param(1), ?param(2), ?param(3),
				 ?param(4)})
    end,
    %io:format("SCAN_TABLE, X: ~w, TABLE: ~w, LEN: ~w Addr: ~w~n",
	%      [?param(1), ?param(2), ?param(3), Addr]),
    ?store_var(Addr),
    ?branch_or_advance(Addr > 0).

set_attr(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?call_machine({object_set_attribute, ?param(1), ?param(2)}).

set_cursor(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?call_machine({set_cursor, ?param(1), ?param(2)}).

set_text_style(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?call_machine({set_text_style, ?param(1)}).

set_window(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?call_machine({set_window, ?param(1)}).

show_status(_Instruction, ServerRef) ->
    ?call_machine(update_status_line).

sound_effect(_Instruction, _ServerRef) -> void. % not implemented

split_window(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?call_machine({split_window, ?param(1)}).

sread(#instruction{operands = Operands, store_variable = StoreVar},
      ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    MachineStatus = ?call_machine(status),
    %io:format("Status: ~p~n", [MachineStatus]),
    if
	MachineStatus =:= run ->
	    % if this is the first invocation of the READ instruction,
	    % change machine status to sread
            ?call_machine({set_status, sread});
        true ->
	    % this is the second invocation, parse the input
	    % and write the results into the buffers
	    ?call_machine({sread, ?param(1), ?param(2), StoreVar}),
            ?call_machine({set_status, run})
    end.

store(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?store_var2(?param(1), ?param(2)).

storeb(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?call_machine({set_byte, ?param(1) + ?param(2), ?param(3)}).

storew(#instruction{operands = Operands}, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?call_machine({set_word16, ?param(1) + ?param(2) * 2, ?param(3)}).

sub(#instruction{operands = Operands, store_variable = StoreVar},
    ServerRef) ->
    ?USE_SIGNED_PARAMETERS,
    % see @add
    Diff = ?param(1) - ?param(2),
    ?store_var(?trunc16(Diff)).

test(#instruction{operands = Operands} = Instruction, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?branch_or_advance((?param(1) band ?param(2)) =:= ?param(2)).

test_attr(#instruction{operands = Operands} = Instruction, ServerRef) ->
    ?USE_UNSIGNED_PARAMETERS,
    ?branch_or_advance(?call_machine({object_has_attribute,
				     ?param(1), ?param(2)})).

verify(Instruction, ServerRef) -> ?branch_or_advance(true). % do not check

op_halt(_Operands, ServerRef) -> ?call_machine({set_status, halt}).

%%%-----------------------------------------------------------------------
%%% Branch execution
%%%-----------------------------------------------------------------------

branch_or_advance(ServerRef, #instruction{opcode_length = OpcodeLength,
					     branch_info = BranchInfo},
                  Condition) ->
    #branch_info{branch_on_true = BranchOnTrue} = BranchInfo,
    if
	Condition =:= BranchOnTrue ->
	    do_branch(ServerRef, BranchInfo);
	true ->
	    ?next_instruction()
    end.

% Performs the branch according to the specification
do_branch(ServerRef,
	  #branch_info{offset = 0})                                         ->
    ?return_from_routine(?FALSE);
do_branch(ServerRef,
	  #branch_info{offset = 1})                                         ->
    ?return_from_routine(?TRUE);
do_branch(ServerRef,
	  #branch_info{offset = Offset,
		       address_after_branch_data = AddressAfterBranchData}) ->
    ?call_machine({set_pc, AddressAfterBranchData + Offset - 2}).
  
%%%-----------------------------------------------------------------------
%%% Parameter extraction
%%%-----------------------------------------------------------------------

% Extracts a ParameterList from the
% specified list of operands. This is the standard method to use when the
% signum of all parameters is irrelevant
params(ServerRef, Operands) ->
    params_from_operands(ServerRef, Operands, fun operand_value/2).

% similar to params, but instead, the list is assumed to contain 16 bit
% signed integer values 
signed_params(ServerRef, Operands) ->
    params_from_operands(ServerRef, Operands, fun signed_operand_value/2).

% create a list of parameters from the specified operands.
params_from_operands(ServerRef, Operands, Extract) ->
    lists:map(fun(Operand) -> Extract(ServerRef, Operand) end, Operands).

% Returns the value of the specified operand, depending on the operand
% type.
operand_value(ServerRef, {variable, Value})     ->
    ?call_machine({get_var, Value});
operand_value(_ServerRef, {_OperandType, Value}) -> Value.

% Returns the signed operand value
signed_operand_value(ServerRef, Operand) ->
    util:unsigned_to_signed16(operand_value(ServerRef, Operand)).

%%%-----------------------------------------------------------------------
%%% Printing Instructions debugging support
%%%-----------------------------------------------------------------------

print_instr(ServerRef,
	    #instruction{operand_count = OperandCount,
			 opcode_num = OpcodeNum, operands = Operands,
			 store_variable = StoreVariable,
			 address = Address, opcode_length = OpcodeLength},
	    Num) ->
    Version = ?call_machine(version),
    io:format("~p - ~.16X(~p): ~p (oc: ~.16X)",
	      [ Num, Address, "$", OpcodeLength,
		op_name(OperandCount, Version, OpcodeNum), OpcodeNum, "#$" ]),
    print_operands(Operands),
    print_storevar(StoreVariable),
    io:fwrite("\n").

print_operands([])                     -> [];
print_operands([{omitted, _}])         -> [];
print_operands([ Operand | Operands ]) ->
    format_param(Operand), 
    print_operands(Operands).

format_param({variable, Value})       ->
    io:format(" (Var, ~.16X) ", [Value, "#$"]);
format_param({small_constant, Value}) ->
    io:format(" (Small, ~.16X) ", [Value, "#$"]);
format_param({large_constant, Value}) ->
    io:format(" (Large, ~.16X) ", [Value, "#$"]);
format_param({zscii, Value}) ->
    io:format(" ~p ", [Value]).

print_storevar(undef)         -> undef;
print_storevar(StoreVariable) -> io:format(" -> ~.16X", [StoreVariable, "#$"]).

op_name(oc_ext, Version, OpcodeNum) -> oc_ext_op_name(Version, OpcodeNum);
op_name(oc_var, Version, OpcodeNum) -> oc_var_op_name(Version, OpcodeNum);
op_name(oc_2op, Version, OpcodeNum) -> oc_2op_op_name(Version, OpcodeNum);
op_name(oc_0op, Version, OpcodeNum) -> oc_0op_op_name(Version, OpcodeNum);
op_name(oc_1op, Version, OpcodeNum) -> oc_1op_op_name(Version, OpcodeNum).

oc_0op_op_name(_, OpcodeNum) ->
    case OpcodeNum of
	?NEW_LINE       -> new_line;
	?NOP            -> nop;
	?PIRACY         -> piracy;
	?POP            -> pop;
	?PRINT          -> print;
	?PRINT_RET      -> print_ret;
	?QUIT           -> quit;
	?RET_POPPED     -> ret_popped;
	?RFALSE         -> rfalse;
	?RTRUE          -> rtrue;
	?SHOW_STATUS    -> show_status;
	?VERIFY         -> verify;
	_Default        -> '??? (0OP)'
    end.

oc_1op_op_name(Version, ?CALL_1N) when Version >= 5 -> call_1n;
oc_1op_op_name(_, OpcodeNum) ->
    case OpcodeNum of
	?CALL_1S        -> call_1s;
	?DEC            -> dec;
	?GET_CHILD      -> get_child;
	?GET_PARENT     -> get_parent;
	?GET_PROP_LEN   -> get_prop_len;
	?GET_SIBLING    -> get_sibling;
	?INC            -> inc;
	?JUMP           -> jump;
	?JZ             -> jz;
	?LOAD           -> load;
	?NOT            -> 'not';
	?PRINT_ADDR     -> print_addr;
	?PRINT_OBJ      -> print_obj;
	?PRINT_PADDR    -> print_paddr;
	?REMOVE_OBJ     -> remove_obj;
	?RET            -> ret;
	_Default        -> '??? (1OP)'
    end.

oc_2op_op_name(_, OpcodeNum) ->
    case OpcodeNum of
	?ADD            -> add;
	?AND            -> 'and';
	?CALL_2N        -> call_2n;
	?CALL_2S        -> call_2s;
	?CLEAR_ATTR     -> clear_attr;
	?DEC_CHK        -> dec_chk;
	?DIV            -> 'div';
	?GET_NEXT_PROP  -> get_next_prop;
	?GET_PROP       -> get_prop;
	?GET_PROP_ADDR  -> get_prop_addr;
	?INC_CHK        -> inc_chk;
	?INSERT_OBJ     -> insert_obj;
	?JE             -> je;
	?JIN            -> jin;
	?JG             -> jg;
	?JL             -> jl;
	?LOADB          -> loadb;
	?LOADW          -> loadw;
	?MOD            -> mod;
	?MUL            -> mul;
	?OR             -> 'or';
	?SET_ATTR       -> set_attr;
	?SUB            -> sub;
	?STORE          -> store;
	?TEST           -> test;
	?TEST_ATTR      -> test_attr;
	_Default        -> '??? (2OP)'
    end.

oc_var_op_name(Version, ?CALL_VN) when Version >= 5  -> call_vn;
oc_var_op_name(Version, ?CALL_VN2) when Version >= 5 -> call_vn2;
oc_var_op_name(Version, ?AREAD) when Version >= 5    -> aread;
oc_var_op_name(_, OpcodeNum) ->
    case OpcodeNum of
	?BUFFER_MODE    -> buffer_mode;
	?CALL           -> call;
	?CALL_VS2       -> call_vs2;
	?ERASE_WINDOW   -> erase_window;
	?OUTPUT_STREAM  -> output_stream;
	?PRINT_CHAR     -> print_char;
	?PRINT_NUM      -> print_num;
	?PUSH           -> push;
	?PULL           -> pull;
	?PUT_PROP       -> put_prop;
	?RANDOM         -> random;
	?READ_CHAR      -> read_char;
	?SCAN_TABLE     -> scan_table;
	?SET_CURSOR     -> set_cursor;
	?SET_TEXT_STYLE -> set_text_style;
	?SET_WINDOW     -> set_window;
	?SOUND_EFFECT   -> sound_effect;
	?SPLIT_WINDOW   -> split_window;
	?SREAD          -> sread;
	?STOREB         -> storeb;
	?STOREW         -> storew;
	_Default        -> '??? (VAR)'
    end.

oc_ext_op_name(_, OpcodeNum) ->
    case OpcodeNum of
	?SAVE_UNDO      -> save_undo;
	_Default        -> '??? (EXT)'
    end.
