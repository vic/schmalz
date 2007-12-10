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
-export([execute/2, print_instr/3]).
-include("include/glulx.hrl").
-define(OPERAND_VALUE(Num), operand_value(MachinePid, Operands, Num)).
-define(BYTE_OPERAND_VALUE(Num), byte_operand_value(MachinePid, Operands, Num)).
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
    KeepPC = keep_pc(OpcodeNum),
    if 
	KeepPC -> keep_pc;
	true   -> ?call_machine({inc_pc, Length})
    end.

get_operation(OpcodeNum) ->
    case OpcodeNum of
	?ADD          -> fun add/2;
	?ALOAD        -> fun aload/2;
	?ALOADB       -> fun aloadb/2;
	?ALOADBIT     -> fun aloadbit/2;
	?ALOADS       -> fun aloads/2;
	?ASTORE       -> fun astore/2;
	?ASTOREBIT    -> fun astorebit/2;
	?BINARYSEARCH -> fun binarysearch/2;
	?BITAND       -> fun bitand/2;
	?CALL         -> fun call/2;
	?CALLF        -> fun callf/2;
	?CALLFI       -> fun callfi/2;
	?CALLFII      -> fun callfii/2;
	?CALLFIII     -> fun callfiii/2;
	?COPY         -> fun copy/2;
	?COPYB        -> fun copyb/2;
	?GESTALT      -> fun gestalt/2;
	?GETMEMSIZE   -> fun getmemsize/2;
	?GLK          -> fun glk/2;
	?JEQ          -> fun jeq/2;
	?JGE          -> fun jge/2;
	?JGEU         -> fun jgeu/2;
	?JGT          -> fun jgt/2;
	?JLT          -> fun jlt/2;
	?JNE          -> fun jne/2;
	?JNZ          -> fun jnz/2;
	?JUMP         -> fun jump/2;
	?JZ           -> fun jz/2;
	?MUL          -> fun mul/2;
	?NOP          -> fun nop/2;
	?RETURN       -> fun return/2;
	?SETIOSYS     -> fun setiosys/2;
	?SUB          -> fun sub/2;
	?STKCOPY      -> fun stkcopy/2;
	?STREAMCHAR   -> fun streamchar/2;
	?STREAMNUM    -> fun streamnum/2;
	?STREAMSTR    -> fun streamstr/2;
	_Default      -> fun halt/2
    end.

% Branches
keep_pc(?JEQ)      -> true;
keep_pc(?JGE)      -> true;
keep_pc(?JGEU)     -> true;
keep_pc(?JGT)      -> true;
keep_pc(?JLT)      -> true;
keep_pc(?JNE)      -> true;
keep_pc(?JNZ)      -> true;
keep_pc(?JUMP)     -> true;
keep_pc(?JZ)       -> true;
% Calls
keep_pc(?CALL)     -> true;
keep_pc(?CALLF)    -> true;
keep_pc(?CALLFI)   -> true;
keep_pc(?CALLFII)  -> true;
keep_pc(?CALLFIII) -> true;
% Return
keep_pc(?RETURN)   -> true;
keep_pc(_)         -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Instructions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add(MachinePid, #instr{operands = Operands}) ->
    ?call_machine({store_value, ?OPERAND_VALUE(1) + ?OPERAND_VALUE(2),
		   ?OPERAND(3)}).

aload(MachinePid, #instr{operands = Operands}) ->
    Value = ?call_machine({get_word32, ?OPERAND_VALUE(1) +
			   4 * ?OPERAND_VALUE(2)}),
    ?call_machine({store_value, Value, ?OPERAND(3)}).

aloadb(MachinePid, #instr{operands = Operands}) ->
    Value = ?call_machine({get_byte, ?OPERAND_VALUE(1) + ?OPERAND_VALUE(2)}),
    ?call_machine({store_value, Value, ?OPERAND(3)}).

aloadbit(MachinePid, #instr{operands = Operands}) ->
    {ByteAddr, BitNum} = bit_pos(?OPERAND_VALUE(1), ?SIGNED_OPERAND_VALUE(2)),
    Byte = ?call_machine({get_byte, ByteAddr}),
    BitMask = 2#10000000 bsr (7 - BitNum),
    case Byte band BitMask of
	BitMask  -> ?call_machine({store_value, 1, ?OPERAND(3)});
	_Default -> ?call_machine({store_value, 0, ?OPERAND(3)})
    end.

%% Helper method used by aloadbit and astorebit, given a base address and
%% offset, determine the byte address and bit number in memory
%% @spec bit_pos(int(), int()) -> {int(), int()}.
bit_pos(Base, Offset) ->
    if
	Offset < 0 ->	
	    ByteAddr = (Base + Offset div 8) - 1,
	    BitNum = (Offset rem 8 + 8) band 7;
	true   ->
	    ByteAddr = Base + Offset div 8,
	    BitNum = Offset rem 8
    end,
    {ByteAddr, BitNum}.
    
aloads(MachinePid, #instr{operands = Operands}) ->
    Value = ?call_machine({get_word16, ?OPERAND_VALUE(1) +
			   2 * ?OPERAND_VALUE(2)}),
    ?call_machine({store_value, Value, ?OPERAND(3)}).

astore(MachinePid, #instr{operands = Operands}) ->
    ?call_machine({set_word32, ?OPERAND_VALUE(1) + 4 * ?OPERAND_VALUE(2),
		   ?OPERAND_VALUE(3)}).

astorebit(MachinePid, #instr{operands = Operands}) ->
    {ByteAddr, BitNum} = bit_pos(?OPERAND_VALUE(1), ?SIGNED_OPERAND_VALUE(2)),
    Byte = ?call_machine({get_byte, ByteAddr}),
    BitMask = 2#10000000 bsr (7 - BitNum),
    case ?OPERAND_VALUE(3) of
	0        ->
	    ClearMask = (bnot BitMask) band 16#ff,
	    SetByte = Byte band ClearMask;
	_Default ->
	    SetByte = Byte bor BitMask
    end,
    ?call_machine({set_byte, ByteAddr, SetByte band 16#ff}).

binarysearch(MachinePid, #instr{operands = Operands}) ->
    Result = ?call_machine({binarysearch, ?OPERAND_VALUE(1), ?OPERAND_VALUE(2),
			   ?OPERAND_VALUE(3), ?OPERAND_VALUE(4),
			   ?OPERAND_VALUE(5), ?OPERAND_VALUE(6),
			   ?OPERAND_VALUE(7)}),
    ?call_machine({store_value, Result, ?OPERAND(8)}).

bitand(MachinePid, #instr{operands = Operands}) ->
    ?call_machine({store_value, ?OPERAND_VALUE(1) band ?OPERAND_VALUE(2),
		   ?OPERAND(3)}).

call(MachinePid, #instr{operands = Operands, address = InstrAddress,
		        length = InstrLength}) ->
    ?call_machine({call, ?OPERAND_VALUE(1), ?OPERAND_VALUE(2),
		   InstrAddress + InstrLength, ?OPERAND(3)}).

callf(MachinePid, #instr{operands = Operands, address = InstrAddress,
			 length = InstrLength}) ->
    ?call_machine({callf, ?OPERAND_VALUE(1), [],
		   InstrAddress + InstrLength, ?OPERAND(2)}).

callfi(MachinePid, #instr{operands = Operands, address = InstrAddress,
			  length = InstrLength}) ->
    ?call_machine({callf, ?OPERAND_VALUE(1), [?OPERAND_VALUE(2)],
		   InstrAddress + InstrLength, ?OPERAND(3)}).

callfii(MachinePid, #instr{operands = Operands, address = InstrAddress,
			   length = InstrLength}) ->
    ?call_machine({callf, ?OPERAND_VALUE(1),
		   [?OPERAND_VALUE(2), ?OPERAND_VALUE(3)],
		   InstrAddress + InstrLength, ?OPERAND(4)}).

callfiii(MachinePid, #instr{operands = Operands, address = InstrAddress,
			    length = InstrLength}) ->
    ?call_machine({callf, ?OPERAND_VALUE(1),
		   [?OPERAND_VALUE(2), ?OPERAND_VALUE(3), ?OPERAND_VALUE(4)],
		   InstrAddress + InstrLength, ?OPERAND(5)}).

copy(MachinePid, #instr{operands = Operands}) ->
    ?call_machine({store_value, ?OPERAND_VALUE(1), ?OPERAND(2)}).

copyb(MachinePid, #instr{operands = Operands}) ->
    ?call_machine({store_byte_value, ?BYTE_OPERAND_VALUE(1), ?OPERAND(2)}).

getmemsize(MachinePid, #instr{operands = Operands}) ->
    ?call_machine({store_value, ?call_machine(memsize), ?OPERAND(1)}).

gestalt(MachinePid, #instr{operands = Operands}) ->
    ?call_machine({store_value,
		   ?call_machine({gestalt, ?OPERAND_VALUE(1),
				  ?OPERAND_VALUE(2)}),
		   ?OPERAND(3)}).

glk(MachinePid, #instr{operands = Operands}) ->
    ?call_machine({store_value,
		   ?call_machine({glk, ?OPERAND_VALUE(1),
				  ?OPERAND_VALUE(2)}),
		   ?OPERAND(3)}).

jeq(MachinePid, #instr{operands = Operands} = Instruction) ->
    ?branch_or_advance(?SIGNED_OPERAND_VALUE(1) =:= ?SIGNED_OPERAND_VALUE(2),
		       ?OPERAND_VALUE(3)).

jge(MachinePid, #instr{operands = Operands} = Instruction) ->
    ?branch_or_advance(?SIGNED_OPERAND_VALUE(1) >= ?SIGNED_OPERAND_VALUE(2),
			?OPERAND_VALUE(3)).

jgeu(MachinePid, #instr{operands = Operands} = Instruction) ->
     ?branch_or_advance(?OPERAND_VALUE(1) >= ?OPERAND_VALUE(2),
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

jnz(MachinePid, #instr{operands = Operands} = Instruction) ->
    ?branch_or_advance(?OPERAND_VALUE(1) /= 0, ?OPERAND_VALUE(2)).

jump(MachinePid, #instr{operands = Operands} = Instruction) ->
    ?branch_or_advance(true, ?OPERAND_VALUE(1)).

jz(MachinePid, #instr{operands = Operands} = Instruction) ->
   ?branch_or_advance(?OPERAND_VALUE(1) =:= 0, ?OPERAND_VALUE(2)).

mul(MachinePid, #instr{operands = Operands}) ->
    ?call_machine({store_value, ?OPERAND_VALUE(1) * ?OPERAND_VALUE(2),
		   ?OPERAND(3)}).

nop(_MachinePid, _Instruction) -> nop.

return(MachinePid, #instr{operands = Operands}) ->
    ?call_machine({return_from_call, ?OPERAND_VALUE(1)}).

setiosys(MachinePid, #instr{operands = Operands}) ->
    ?call_machine({set_iosys, ?OPERAND_VALUE(1), ?OPERAND_VALUE(2)}).

sub(MachinePid, #instr{operands = Operands}) ->
    ?call_machine({store_value, ?OPERAND_VALUE(1) - ?OPERAND_VALUE(2),
		   ?OPERAND(3)}).

stkcopy(MachinePid, #instr{operands = Operands}) ->
    ?call_machine({stack_copy, ?OPERAND_VALUE(1)}).

streamchar(MachinePid, #instr{operands = Operands}) ->
    ?call_machine({streamchar, ?OPERAND_VALUE(1)}).

streamnum(MachinePid, #instr{operands = Operands}) ->
    ?call_machine({streamnum, ?OPERAND_VALUE(1)}).

streamstr(MachinePid, #instr{operands = Operands}) ->
    ?call_machine({streamstr, ?OPERAND_VALUE(1)}).

branch_or_advance(MachinePid, false, _Offset, Instruction) ->
    ?call_machine({inc_pc, Instruction#instr.length});
branch_or_advance(MachinePid, true, 0, _Instruction) ->
    ?call_machine({return_from_call, 0});
branch_or_advance(MachinePid, true, 1, _Instruction) ->
    ?call_machine({return_from_call, 1});
branch_or_advance(MachinePid, true, Offset,
		  #instr{address = InstrAddress, length = Length,
			 opnum_len = _OpNumLen}) ->
    %io:format("branch, InstrAddr: ~w, Length: ~w, OpNumLen: ~w, Offset = ~w~n",
%	      [InstrAddress, Length, OpNumLen, Offset]),
    ?call_machine({set_pc, InstrAddress + Length + Offset - 2}).

operand_value(MachinePid, Operands, Num) ->
    {Type, Value} = ?OPERAND(Num),
    case Type of
	const    -> Value;
	stack    -> ?pop();
	memory   -> ?call_machine({get_word32, Value});
	local    -> ?call_machine({get_local, Value});
	ram      -> ?call_machine({get_ram_word32, Value});
	_Default -> undef
    end.

byte_operand_value(MachinePid, Operands, Num) ->
    {Type, _} = ?OPERAND(Num),
    Value32 = operand_value(MachinePid, Operands, Num),
    if
	Type =:= stack ->
	    Value32 band 16#ff;
	true ->
	    (Value32 bsr 24) band 16#ff
    end.
    
halt(MachinePid, _) ->
    ?call_machine({set_status, halt}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% print_instr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_instr(MachinePid, #instr{opcode = OpcodeNum, address = Address,
		   operands = Operands}, Num) ->
    io:format("~p - $~8.16.0B @~w ~s St: ~p ~n",
	      [Num, Address, op_name(OpcodeNum), operands_str(Operands),
	       ?call_machine(get_stack)]).

operands_str([]) -> "";
operands_str([{const, Value} | Operands]) ->
    io_lib:format("#$~8.16.0B ", [Value]) ++ operands_str(Operands);
operands_str([{stack, _} | Operands]) ->
    io_lib:format("(SP) ", []) ++ operands_str(Operands);
operands_str([{memory, Value} | Operands]) ->
    io_lib:format("MEM~8.16.0B ", [Value]) ++ operands_str(Operands);
operands_str([{local, Value} | Operands]) ->
    io_lib:format("L~8.16.0B ", [Value]) ++ operands_str(Operands);
operands_str([{ram, Value} | Operands]) ->
    io_lib:format("RAM~8.16.0B ", [Value]) ++ operands_str(Operands).

%% For debugging purposes
op_name(OpcodeNum) ->
    case OpcodeNum of
	?ADD          -> add;
	?ALOAD        -> aload;
	?ALOADB       -> aloadb;
	?ALOADBIT     -> aloadbit;
	?ALOADS       -> aloads;
	?ASTORE       -> astore;
	?ASTOREBIT    -> astorebit;
	?BINARYSEARCH -> binarysearch;
	?BITAND       -> bitand;
	?CALL         -> call;
	?CALLF        -> callf;
	?CALLFI       -> callfi;
	?CALLFII      -> callfii;
	?CALLFIII     -> callfiii;
	?COPY         -> copy;
	?COPYB        -> copyb;
	?GESTALT      -> gestalt;
	?GETMEMSIZE   -> getmemsize;
	?GLK          -> glk;
	?JEQ          -> jeq;
	?JGE          -> jge;
	?JGEU         -> jgeu;
	?JGT          -> jgt;
	?JLT          -> jlt;
	?JNE          -> jne;
	?JNZ          -> jnz;
	?JUMP         -> jump;
	?JZ           -> jz;
	?MUL          -> mul;
	?NOP          -> nop;
	?RETURN       -> return;
	?SETIOSYS     -> setiosys;
	?SUB          -> sub;
	?STKCOPY      -> stkcopy;
	?STREAMCHAR   -> streamchar;
	?STREAMNUM    -> streamnum;
	?STREAMSTR    -> streamstr;
	_Default      -> '???'
    end.
