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
%%% Description of module glulx_instr
%%%-----------------------------------------------------------------------
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%%
%%%-----------------------------------------------------------------------

-module(glulx_instr).
-export([execute/2, print_instr/2]).
-include("include/glulx.hrl").
-define(OPERAND_VALUE(Num), operand_value(MachinePid, Operands, Num)).
-define(SIGNED_OPERAND_VALUE(Num),
	glulx_util:unsigned_to_signed32(operand_value(MachinePid, Operands,
						      Num))).
-define(OPERAND(Num), lists:nth(Num, Operands)).
-define(branch_or_advance(Condition, Offset),
        branch_or_advance(MachinePid, Condition, Offset, Instruction)).

execute(MachinePid, #instr{opcode = OpcodeNum, length = Length}
	= Instruction) ->
    Execute = get_operation(OpcodeNum),
    Execute(MachinePid, Instruction),
    IsCall = is_call(OpcodeNum),
    IsBranch = is_branch(OpcodeNum),
    IsReturn = is_return(OpcodeNum),
    if 
	IsCall; IsBranch; IsReturn -> keep_pc;
	true                       -> ?call_machine({inc_pc, Length})
    end.

get_operation(OpcodeNum) ->
    case OpcodeNum of
	?ALOAD      -> fun aload/2;
	?ALOADB     -> fun aloadb/2;
	?CALL       -> fun call/2;
	?CALLFI     -> fun callfi/2;
	?CALLFII    -> fun callfii/2;
	?COPY       -> fun copy/2;
	?GETMEMSIZE -> fun getmemsize/2;
	?JEQ        -> fun jeq/2;
	?JGEU       -> fun jgeu/2;
	?JGT        -> fun jgt/2;
	?JLT        -> fun jlt/2;
	?JNE        -> fun jne/2;
	?JUMP       -> fun jump/2;
	?NOP        -> fun nop/2;
	?RETURN     -> fun return/2;
	?SUB        -> fun sub/2;
	_Default    -> fun halt/2
    end.

is_branch(?JEQ)    -> true;
is_branch(?JGEU)   -> true;
is_branch(?JGT)    -> true;
is_branch(?JLT)    -> true;
is_branch(?JNE)    -> true;
is_branch(_)       -> false.

is_call(?CALL)     -> true;
is_call(?CALLFI)   -> true;
is_call(?CALLFII)  -> true;
is_call(_)         -> false.

is_return(?RETURN) -> true;
is_return(_)       -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Instructions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

aload(MachinePid, #instr{operands = Operands, address = InstrAddress,
			  length = InstrLength}) ->
    Value = ?call_machine({get_word32, ?OPERAND_VALUE(1) +
			   ?OPERAND_VALUE(2) * 4}),
    ?call_machine({store_value, Value, ?OPERAND(3)}).

aloadb(MachinePid, #instr{operands = Operands, address = InstrAddress,
			  length = InstrLength}) ->
    Value = ?call_machine({get_byte, ?OPERAND_VALUE(1) + ?OPERAND_VALUE(2)}),
    ?call_machine({store_value, Value, ?OPERAND(3)}).

call(MachinePid, #instr{operands = Operands, address = InstrAddress,
		        length = InstrLength}) ->
    ?call_machine({call, ?OPERAND_VALUE(1), ?OPERAND_VALUE(2),
		   InstrAddress + InstrLength, ?OPERAND(3)}).

callfi(MachinePid, #instr{operands = Operands, address = InstrAddress,
			  length = InstrLength}) ->
    ?call_machine({callf, ?OPERAND_VALUE(1), [?OPERAND_VALUE(2)],
		   InstrAddress + InstrLength, ?OPERAND(3)}).

callfii(MachinePid, #instr{operands = Operands, address = InstrAddress,
			   length = InstrLength}) ->
    ?call_machine({callf, ?OPERAND_VALUE(1),
		   [?OPERAND_VALUE(2), ?OPERAND_VALUE(3)],
		   InstrAddress + InstrLength, ?OPERAND(4)}).

copy(MachinePid, #instr{operands = Operands}) ->
    ?call_machine({store_value, ?OPERAND_VALUE(1), ?OPERAND(2)}).

getmemsize(MachinePid, #instr{operands = Operands}) ->
    ?call_machine({store_value, ?call_machine(memsize), ?OPERAND(1)}).

jeq(MachinePid, #instr{operands = Operands} = Instruction) ->
    ?branch_or_advance(?SIGNED_OPERAND_VALUE(1) =:= ?SIGNED_OPERAND_VALUE(2),
		       ?OPERAND_VALUE(3)).

jgeu(MachinePid, #instr{operands = Operands} = Instruction) ->
     ?branch_or_advance(?OPERAND_VALUE(1) =:= ?OPERAND_VALUE(2),
			?OPERAND_VALUE(3)).

jgt(MachinePid, #instr{operands = Operands} = Instruction) ->
    ?branch_or_advance(?SIGNED_OPERAND_VALUE(1) > ?SIGNED_OPERAND_VALUE(2),
		      ?OPERAND_VALUE(3)).

jlt(MachinePid, #instr{operands = Operands} = Instruction) ->
    ?branch_or_advance(?SIGNED_OPERAND_VALUE(1) < ?SIGNED_OPERAND_VALUE(2),
		      ?OPERAND_VALUE(3)).

jne(MachinePid, #instr{operands = Operands} = Instruction) ->
    ?branch_or_advance(?SIGNED_OPERAND_VALUE(1) /= ?SIGNED_OPERAND_VALUE(2),
		       ?OPERAND_VALUE(3)).

jump(MachinePid, #instr{operands = Operands} = Instruction) ->
    ?branch_or_advance(true, ?OPERAND_VALUE(1)).

nop(_MachinePid, _Instruction) -> nop.

return(MachinePid, #instr{operands = Operands}) ->
    ?call_machine({return_from_call, ?OPERAND_VALUE(1)}).

sub(MachinePid, #instr{operands = Operands}) ->
    ?call_machine({store_value, ?OPERAND_VALUE(1) - ?OPERAND_VALUE(2),
		   ?OPERAND(3)}).

branch_or_advance(MachinePid, false, _Offset, Instruction) ->
    ?call_machine({inc_pc, Instruction#instr.length});
branch_or_advance(MachinePid, true, 0, _Instruction) ->
    ?call_machine({return_from_call, 0});
branch_or_advance(MachinePid, true, 1, _Instruction) ->
    ?call_machine({return_from_call, 1});
branch_or_advance(MachinePid, true, Offset,
		  #instr{address = InstrAddress, length = Length,
			 opnum_len = OpNumLen}) ->
    io:format("branch, InstrAddr: ~w, Length: ~w, OpNumLen: ~w, Offset = ~w~n",
	      [InstrAddress, Length, OpNumLen, Offset]),
    ?call_machine({set_pc, InstrAddress + Length + Offset - 2}).

operand_value(MachinePid, Operands, Num) ->
    {Type, Value} = ?OPERAND(Num),
    case Type of
	stack    -> ?pop();
	local    -> ?call_machine({get_local, Value});
	_Default -> Value
    end.

halt(MachinePid, _) ->
    ?call_machine({set_status, halt}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% print_instr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_instr(#instr{opcode = OpcodeNum, address = Address,
		   operands = Operands}, Num) ->
    io:format("~p - $~8.16.0B @~w ~s~n", [Num, Address, op_name(OpcodeNum),
					  operands_str(Operands)]).

operands_str([]) -> "";
operands_str([{const, Value} | Operands]) ->
    io_lib:format("#$~8.16.0B ", [Value]) ++ operands_str(Operands);
operands_str([{stack, _} | Operands]) ->
    io_lib:format("(SP) ", []) ++ operands_str(Operands);
operands_str([{local, Value} | Operands]) ->
    io_lib:format("L~8.16.0B ", [Value]) ++ operands_str(Operands).

op_name(OpcodeNum) ->
    case OpcodeNum of
	?ALOAD      -> aload;
	?ALOADB     -> aloadb;
	?BITAND     -> bitand;
	?CALL       -> call;
	?CALLFI     -> callfi;
	?CALLFII    -> callfii;
	?COPY       -> copy;
	?GETMEMSIZE -> getmemsize;
	?JEQ        -> jeq;
	?JGEU       -> jgeu;
	?JGT        -> jgt;
	?JLT        -> jlt;
	?JNE        -> jne;
	?JUMP       -> jump;
	?NOP        -> nop;
	?RETURN     -> return;
	?SUB        -> sub;
	_Default    -> '???'
    end.
