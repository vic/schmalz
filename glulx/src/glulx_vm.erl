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
%%% Description of module glulx_vm
%%%-----------------------------------------------------------------------
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%%
%%%-----------------------------------------------------------------------

-module(glulx_vm).
-export([create/1, rpc/2, glk_state/1, set_glk_state/2,
	 set_byte/3, set_word32/3, halt_vm/2, push/2, pop/1]).
-include("include/glulx.hrl").
-include("include/glk.hrl").

-record(call_frame, {type, result_spec, return_address, invocation_sp,
		     locals}).
-record(glulx_vm, {memory, call_stack, value_stack, pc, status, glk_state}).

% Function types
-define(ARGS_STACK,  192).
-define(ARGS_LOCALS, 193).

% Destination types for function calls
-define(DO_NOT_STORE,         0).
-define(STORE_IN_MAIN_MEMORY, 1).
-define(STORE_IN_LOCAL_VAR,   2).
-define(STORE_ON_STACK,       3).

%% Creates an initialized Glulx-VM.
%% @spec create(GlulxMem()) -> pid().
create(Memory) ->
    #glulx_header{start_func = StartFuncAddr} = glulx_mem:header(Memory),
    StartFuncFrame = decode_function(Memory, StartFuncAddr, 0, undef,
				     {const, ?DO_NOT_STORE}),
    Vm = #glulx_vm{memory = Memory, value_stack = [],
		   call_stack = [StartFuncFrame],
		   pc = StartFuncAddr + function_offset(StartFuncFrame),
		   status = run, glk_state = glk:init()},
    spawn(fun() -> listen(Vm) end).

%% Returns the glk state
glk_state(MachineState) -> MachineState#glulx_vm.glk_state.

%% Sets the Glk state
set_glk_state(VmState0, GlkState) ->
    VmState0#glulx_vm{glk_state = GlkState}.

rpc(MachinePid, Message) ->
    MachinePid ! {self(), Message},
    receive
	{MachinePid, Response} -> Response
    after 500 ->
        io:format("waiting for ack timed out"),
	halt
    end.

%% Halts the VM with an error message
halt_vm(MachineState0, Message) ->
    io:format("Virtual Machine HALT: ~s~n", [Message]),
    MachineState0#glulx_vm{status = halt}.

listen(#glulx_vm{memory = Memory, value_stack = ValueStack, pc = PC,
		 status = Status} = MachineState0) ->
    receive
	{From, pc} ->
	    ack(From, PC),
	    listen(MachineState0);
	{From, {inc_pc, Increment}} ->
	    MachineState1 = MachineState0#glulx_vm{pc = PC + Increment},
	    ack(From, ok),
	    listen(MachineState1);
	{From, {set_pc, NewPC}} ->
	    MachineState1 = MachineState0#glulx_vm{pc = NewPC},
	    ack(From, ok),
	    listen(MachineState1);
	{From, status} ->
	    ack(From, Status),
	    listen(MachineState0);
	{From, {set_status, NewStatus}} ->
	    ack(From, ok),
	    listen(MachineState0#glulx_vm{status = NewStatus});
	{From, {call, Address, NumParams, ReturnAddress, ResultSpec}} ->
	    ack(From, ok),
	    MachineState1 = call(MachineState0, Address, NumParams,
				 ReturnAddress, ResultSpec),
	    listen(MachineState1);
	{From, {callf, Address, Args, ReturnAddress, ResultSpec}} ->
	    ack(From, ok),
	    MachineState1 = callf(MachineState0, Address, Args,
				  ReturnAddress, ResultSpec),
	    listen(MachineState1);
	{From, {return_from_call, Result}} ->
	    ack(From, ok),
	    MachineState1 = return_from_call(MachineState0, Result),
	    listen(MachineState1);
	{From, memsize} ->
	    ack(From, glulx_mem:memsize(Memory)),
	    listen(MachineState0);
	{From, {get_byte, Address}} ->
	    ack(From, glulx_mem:get_byte(Memory, Address)),
	    listen(MachineState0);
	{From, {get_word16, Address}} ->
	    ack(From, glulx_mem:get_word16(Memory, Address)),
	    listen(MachineState0);
	{From, {get_word32, Address}} ->
	    ack(From, glulx_mem:get_word32(Memory, Address)),
	    listen(MachineState0);
	{From, {get_ram_word32, Address}} ->
	    ack(From, glulx_mem:get_ram_word32(Memory, Address)),
	    listen(MachineState0);
	{From, {set_byte, Address, Value}} ->
	    MachineState1 = set_byte(MachineState0, Address, Value),
	    ack(From, ok),
	    listen(MachineState1);
	{From, {set_word32, Address, Value}} ->
	    MachineState1 = set_word32(MachineState0, Address, Value),
	    ack(From, ok),
	    listen(MachineState1);
	{From, pop} ->
	    {StackValue, MachineState1} = pop(MachineState0),
	    ack(From, StackValue),
	    listen(MachineState1);
	{From, {get_local, Address}} ->
	    ack(From, get_local(MachineState0, Address)),
	    listen(MachineState0);
	{From, get_stack} ->
	    ack(From, ValueStack),
	    listen(MachineState0);
	{From, {store_value, Value, Dest}} ->
	    MachineState1 = store_value(MachineState0, Value, Dest),
	    %[CallFrame|_] = MachineState1#glulx_vm.call_stack,
	    %io:format("CallFrame after: ~p~n", [CallFrame]),
	    ack(From, ok),
	    listen(MachineState1);
	{From, {store_byte_value, Value, Dest}} ->
	    MachineState1 = store_byte_value(MachineState0, Value, Dest),
	    ack(From, ok),
	    listen(MachineState1);
	{From, {binarysearch, Key, KeySize, Start, StructSize, NumStructs,
	        KeyOffset, Options}} ->
	    Result = binarysearch(Memory, Key, KeySize, Start, StructSize,
				  NumStructs, KeyOffset, Options),
	    ack(From, Result),
	    listen(MachineState0);	    
	{From, {stack_copy, NumValues}} ->
	    MachineState1 = stack_copy(MachineState0, NumValues),
	    ack(From, ok),
	    listen(MachineState1);
	{From, {gestalt, Selector, Arg}} ->
	    ack(From, gestalt(Selector, Arg)),
	    listen(MachineState0);
	{From, {set_iosys, Mode, Rock}} ->
	    MachineState1 = set_iosys(MachineState0, Mode, Rock),
	    ack(From, ok),
	    listen(MachineState1);
	{From, {glk, Identifier, NumArgs}} ->
	    {GlkResult, MachineState1} =
		glk(MachineState0, Identifier, NumArgs),
	    ack(From, GlkResult),
	    listen(MachineState1);
	{From, {streamchar, Latin1Char}} ->
	    {_GlkResult, MachineState1} =
		glk:glk_put_char(glulx_vm, MachineState0, [Latin1Char]),
	    ack(From, ok),
	    listen(MachineState1);
	{From, {streamnum, Number}} ->
	    streamnum(MachineState0, Number),
	    ack(From, ok),
	    listen(MachineState0);
	{From, {streamstr, StrAddress}} ->
	    MachineState1 = streamstr(MachineState0, StrAddress),
	    ack(From, ok),
	    listen(MachineState1);
	{From, Other} ->
	    io:format("UNKNOWN GLULX OP: ~p~n", Other),
	    ack(From, {error, Other}),
	    listen(MachineState0)
    end.

ack(Pid, Message) -> Pid ! {self(), Message}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Function calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Call where arguments are on the stack
call(MachineState0, Address, NumParams, ReturnAddress, ResultSpec) ->
    {Args, MachineState1} = pop_stack_values(MachineState0, NumParams),
    CallFrame = decode_function(MachineState1#glulx_vm.memory, Address,
				length(MachineState1#glulx_vm.value_stack),
				ReturnAddress, ResultSpec),
    MachineState2 = push_call_frame(MachineState1, CallFrame, Address),
    MachineState3 = copy_args(MachineState2, Args),
    io:format("CALL WITH #~p PARAMS on stack, ARGS: ~p, STACK AFTER: ~p~n",
	      [NumParams, Args, MachineState3#glulx_vm.value_stack]),
    MachineState3.

%% Pops the specicfied number of elements from the stack.
%% @spec pop_stack_values(glulx_vm(), int()) -> {[int()], glulx_vm()}.
pop_stack_values(#glulx_vm{value_stack = ValueStack} = VmState0, NumValues) ->
    VmState1 = verify_pop_stack(VmState0, NumValues),
    Values = lists:sublist(ValueStack, NumValues),
    if
	length(Values) > 0 ->
	    NewStack = lists:sublist(ValueStack, NumValues + 1,
				     length(ValueStack) - NumValues);
	true ->
	    NewStack = ValueStack
    end,
    {Values, VmState1#glulx_vm{value_stack = NewStack}}.
    
%% Call with explicit arguments
callf(#glulx_vm{memory = Memory, value_stack = ValueStack} = MachineState0,
      Address, Args, ReturnAddress, ResultSpec) ->
    CallFrame = decode_function(Memory, Address, length(ValueStack),
				ReturnAddress, ResultSpec),
    MachineState1 = push_call_frame(MachineState0, CallFrame, Address),
    MachineState2 = copy_args(MachineState1, Args),
%    #glulx_vm{call_stack = [CallFrame2 | _], value_stack = ValueStack2}
%	= MachineState2,
    %io:format("@callf with CallFrame: ~p Stack: ~p~n",
%	      [CallFrame2, ValueStack2]),
    MachineState2.

%% Push a call frame on the call stack and sets the pc to the target address
%% @spec push_call_frame(glulx_vm(), call_frame(), int()) -> glulx_vm().
push_call_frame(#glulx_vm{call_stack = CallStack} = MachineState0, CallFrame,
		Address) -> 
    MachineState0#glulx_vm{pc = Address + function_offset(CallFrame),
			   call_stack = [CallFrame | CallStack]}.

%% Depending on the function type, the parameters are either pushed
%% on the stack or set to the locals
%% @spec copy_arguments(glulx_vm(), int[]) -> glulx_vm().
copy_args(#glulx_vm{call_stack = [#call_frame{type = ?ARGS_STACK}|_ ],
		    value_stack = ValueStack} = MachineState0,
	  Args) ->
    MachineState0#glulx_vm{value_stack = [length(Args) | Args ++ ValueStack]};
copy_args(#glulx_vm{call_stack = [#call_frame{type = ?ARGS_LOCALS,
					      locals = Locals} = CallFrame
				  | CallStack ]} = MachineState0,
	  Args) ->
    MachineState0#glulx_vm{call_stack =
			   [CallFrame#call_frame{
			      locals = set_args_to_locals(
					 Locals, Args)}
			    | CallStack ]}.

%% Sets the specified argument list to the current call frames locals list.
%% @spec set_args_to_locals({int(), int()}[], int[]) -> {int(), int()}[].
set_args_to_locals([], _Args) -> [];
set_args_to_locals(Locals, []) -> Locals;
set_args_to_locals([{4, _} | Locals], [Arg | Args]) ->
    [{4, Arg band 16#ffffffff}] ++ set_args_to_locals(Locals, Args);
set_args_to_locals([{2, _} | Locals], [Arg | Args]) ->
    [{2, Arg band 16#ffff}] ++ set_args_to_locals(Locals, Args);
set_args_to_locals([{1, _} | Locals], [Arg | Args]) ->
    [{1, Arg band 16#ff}] ++ set_args_to_locals(Locals, Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Return from call
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

return_from_call(#glulx_vm{call_stack = [CallFrame | CallStack],
			   value_stack = ValueStack}
     = MachineState0, Result) ->
    ReturnAddress = CallFrame#call_frame.return_address,
    io:format("Restore stack to: ~w~n", [CallFrame#call_frame.invocation_sp]),
    NewValueStack = lists:nthtail(length(ValueStack) -
				  CallFrame#call_frame.invocation_sp,
				  ValueStack),
    MachineState1 =
	MachineState0#glulx_vm{pc = ReturnAddress, call_stack = CallStack,
			       value_stack = NewValueStack},
    store_value(MachineState1, Result, CallFrame#call_frame.result_spec).

pop(#glulx_vm{value_stack = [TopValue | Stack]} = MachineState0) ->
    MachineState1 = verify_pop_stack(MachineState0, 1),
    {TopValue, MachineState1#glulx_vm{value_stack = Stack}}.

push(#glulx_vm{value_stack = Stack} = MachineState0, Value) ->
    MachineState0#glulx_vm{value_stack = [Value|Stack]}.

%% check the stack for underflow before popping
verify_pop_stack(#glulx_vm{value_stack = Stack} = VmState0, NumPopValues) ->
    MinStackpointer = min_stackpointer(VmState0),
    if
	length(Stack) - NumPopValues >= MinStackpointer -> VmState0;
	true -> halt_vm(VmState0, "Stack underflow !!!!")
    end.    
    
min_stackpointer(#glulx_vm{call_stack = []}) -> 0;
min_stackpointer(#glulx_vm{call_stack = [CallFrame | _]}) ->
    CallFrame#call_frame.invocation_sp.

%% Stores a 32 bit value
store_value(MachineState0, _Value, {const, 0}) -> MachineState0;
store_value(MachineState0, Value, {stack, _}) ->
    push(MachineState0, Value);
store_value(MachineState0, Value, {memory, ByteAddress}) ->
    set_word32(MachineState0, ByteAddress, Value);
store_value(MachineState0, Value, {local, Address}) ->
    set_local(MachineState0, Address, Value);
store_value(MachineState0, Value, {ram, RamOffset}) ->
    set_ram_word32(MachineState0, RamOffset, Value).

% Stores an 8 bit value
store_byte_value(MachineState0, _Value, {const, 0}) -> MachineState0;
store_byte_value(MachineState0, Value, {stack, _}) ->
    push(MachineState0, Value);
store_byte_value(MachineState0, Value, {memory, ByteAddress}) ->
    set_byte(MachineState0, ByteAddress, Value);
store_byte_value(MachineState0, Value, {local, Address}) ->
    set_local(MachineState0, Address, Value band 16#ff);
store_byte_value(MachineState0, Value, {ram, RamOffset}) ->
    set_ram_byte(MachineState0, RamOffset, Value).

get_local(#glulx_vm{call_stack  = [#call_frame{locals = Locals} |
				   _CallStack]}, Address) ->
    get_local_value(Locals, Address, 0).

get_local_value([{_Size, Value} | _Locals], _Address, _Address) -> Value;
get_local_value([{Size, _Value} | Locals], Address, CurrentAddress)
  when CurrentAddress < Address ->
    get_local_value(Locals, Address, CurrentAddress + Size);
get_local_value(_, Address, _) ->
    io:format("ILLEGAL ACCESS TO NON-EXISTING LOCAL: ~w~n", [Address]),
    undef.

set_local(#glulx_vm{call_stack  = [CallFrame | CallStack]} = MachineState0,
	  Address, Value) ->
    MachineState0#glulx_vm{call_stack =
			   [routine_set_local(CallFrame, Address, Value) |
			    CallStack]}.

local_index(_Locals, _Address, _Address, CurrentIndex) -> CurrentIndex;
local_index([{Size, _Value}|Locals], Address, CurrentAddress, CurrentIndex)
  when CurrentAddress < Address ->
    local_index(Locals, Address, CurrentAddress + Size, CurrentIndex + 1);
local_index(_, _, _, _) -> undef.
    
% sets the nth local variable to the specified value
routine_set_local(#call_frame{locals = Locals} = CallFrame, Address, Value) ->
    Index = local_index(Locals, Address, 0, 0),
    CallFrame#call_frame{ locals = set_local_list(Locals, Index, Value) }.

% replaces the nth value in the given list with the specified value, index is
% 0-based
set_local_list(Locals, Index, NewValue) ->
    {Size, _OldValue} = lists:nth(Index + 1, Locals),
    lists:append([lists:sublist(Locals, Index), [{Size, NewValue}],
		  lists:sublist(Locals, Index + 2,
				length(Locals) - 1)]).

%% Sets the 32 bit word at the specified RAM address
set_ram_word32(#glulx_vm{memory = Memory} = MachineState0, RamOffset, Value) ->
    MachineState0#glulx_vm{memory = glulx_mem:set_ram_word32(
				      Memory, RamOffset, Value)}.

%% Sets the 32 bit word at the specified RAM address
set_ram_byte(#glulx_vm{memory = Memory} = MachineState0, RamOffset, Value) ->
    MachineState0#glulx_vm{memory = glulx_mem:set_ram_byte(
				      Memory, RamOffset, Value)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Helper fuctionality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% First part of a function call, decodes the function at the specified
%% address.
%% @spec decode_function(GlulxMem(), int()) -> call_frame().
decode_function(Memory, Address, InvocationSP, ReturnAddress,
		ResultSpec) ->
    io:format("Function call, saving stack pointer to: ~w~n", [InvocationSP]),
    #call_frame{type = glulx_mem:get_byte(Memory, Address),
		result_spec = ResultSpec,
		return_address = ReturnAddress,
		locals = read_locals(Memory, Address + 1),
		invocation_sp = InvocationSP}.

%% Creates a list of local variables from the specification at
%% the given address
%% @spec read_locals(GlulxMemory(), int()) -> [{int(), int()}].
read_locals(Memory, Address) ->
    LocalType = glulx_mem:get_byte(Memory, Address),
    LocalCount = glulx_mem:get_byte(Memory, Address + 1),
    case LocalType of
	0 -> [];
	_Default -> [{LocalType, 0} || _ <- lists:seq(1, LocalCount)] ++
			read_locals(Memory, Address + 2)
    end.

function_offset(#call_frame{locals = Locals}) ->
   (locals_size(Locals, -1) + 1) * 2 + 1. 

locals_size([], _Last) -> 0;
locals_size([{LocalType, _} | Locals], Last) ->
    if
	LocalType /= Last ->
	    1 + locals_size(Locals, LocalType);
	true ->
	    locals_size(Locals, LocalType)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Searching
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Extracts the search option flags from the option value
search_flags(Options) ->
    { Options band 16#01 =:= 16#01, Options band 16#02 =:= 16#02,
      Options band 16#04 =:= 16#04 }.

%% Naive implementation of key compare: Key is turned into a list
%% of byte values and then compared with the key at address
%% NOTE: ZeroKeyTerminates is ignored
key_compare(Memory, Key, Address, KeySize,
	    {KeyIndirect, _ZeroKeyTerminates}) ->
    KeyList = search_key_to_list(Memory, Key, KeySize, KeyIndirect),
    key_compare(Memory, KeyList, Address).

key_compare(_Memory, [], _Address) -> 0;
key_compare(Memory, [Byte | Bytes], Address) ->
    CmpByte = glulx_mem:get_byte(Memory, Address),
    io:format("Compare ~w with ~w~n", [Byte, CmpByte]),
    if
	Byte < CmpByte -> -1;
        Byte > CmpByte -> 1;
	true           -> key_compare(Memory, Bytes, Address + 1)
    end.

search_key_to_list(_Memory, Key, 1, false) -> [ Key band 16#ff ];
search_key_to_list(_Memory, Key, 2, false) ->
    [ (Key bsr 8) band 16#ff, Key band 16#ff ];
search_key_to_list(_Memory, Key, 3, false) ->
    [ (Key bsr 16) band 16#ff, (Key bsr 8) band 16#ff, Key band 16#ff ];
search_key_to_list(_Memory, Key, 4, false) ->
    [ (Key bsr 24) band 16#ff, (Key bsr 16) band 16#ff,
      (Key bsr 8) band 16#ff, Key band 16#ff ].				      
% TODO: INDIRECT KEYS !!!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Binary Search
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SEARCH_NOT_FOUND, -1).

%% public interface to binary search
binarysearch(Memory, Key, KeySize, Start, StructSize, NumStructs, KeyOffset,
	     Options) ->
    { KeyIndirect, ZeroKeyTerminates, ReturnIndex } = search_flags(Options),
    io:format("BINARYSEARCH, KEY: ~w KEYSIZE: ~w, START: ~w, "
	      "STRUCTSIZE: ~w, NUMSTRUCTS: ~w, KEYOFFSET: ~w, "
	      "INDIRECT: ~p, ZERO_TERM: ~p, RET_IDX: ~p~n",
	      [Key, KeySize, Start, StructSize, NumStructs,
	       KeyOffset, KeyIndirect, ZeroKeyTerminates, ReturnIndex]),
    Index = binsearch(Memory, Key, 0, NumStructs - 1,
	      {Start, StructSize, KeySize, KeyOffset,
	       {KeyIndirect, ZeroKeyTerminates}}),
    io:format("RESULT OF BINSEARCH: ~w~n", [Index]),
    if
	Index =:= ?SEARCH_NOT_FOUND, ReturnIndex      -> -1;
	Index =:= ?SEARCH_NOT_FOUND, not ReturnIndex  -> 0;
	Index /= ?SEARCH_NOT_FOUND,  not ReturnIndex  ->
	    Start + Index * StructSize;
	true                                          -> Index
    end.

binsearch(_Memory, _Key, Left, Right,
	  {_Start, _StructSize, _KeySize, _KeyOffset, _Options})
  when Left > Right -> -1;
binsearch(Memory, Key, Left, Right,
	  {Start, StructSize, KeySize, KeyOffset, Options}) ->
    Index = (Left + Right) div 2,
    Address = Start + Index * StructSize,
    io:format("binsearch at index: ~w, address: ~w~n", [Index, Address]),
    case key_compare(Memory, Key, Address + KeyOffset, KeySize, Options) of
	1  ->
	    binsearch(Memory, Key, Index + 1, Right,
		      {Start, StructSize, KeySize, KeyOffset, Options});
	-1 ->
	    binsearch(Memory, Key, Left, Index - 1,
		      {Start, StructSize, KeySize, KeyOffset, Options});
	0  ->
	    Index
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Stack operations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stack_copy(#glulx_vm{value_stack = ValueStack} = MachineState0, NumValues) ->
    MachineState0#glulx_vm{value_stack =
			   lists:sublist(ValueStack, NumValues) ++ ValueStack}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Gestalt information
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(GESTALT_IOSYS, 4).

%% Returns results for gestalt
gestalt(?GESTALT_IOSYS, _Arg) -> 1;
gestalt(Selector, _Arg) ->
    io:format("UNKNOWN GESTALT SELECTOR: ~w~n", [Selector]),
    undef.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% I/O system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_iosys(MachineState0, Mode, Rock) ->
    io:format("SETTING IOSYS (TODO, ONLY A DUMMY) MODE: ~w, ROCK: ~w~n",
	      [Mode, Rock]),
    MachineState0.

glk(MachineState0, Identifier, NumArgs) ->
    {GlkArgs, MachineState1} = pop_stack_values(MachineState0, NumArgs),
    glk:dispatch(glulx_vm, MachineState1, Identifier, GlkArgs).

%% Sends the string representation of the specified integer to the
%% active Glk string 
streamnum(MachineState0, Number) ->
    glk:glk_put_string(glulx_vm, MachineState0, integer_to_list(Number)).

%% Sends the string at the specified Address to the active Glk stream
streamstr(#glulx_vm{memory = Memory} = MachineState0, StrAddress) ->
    TypeByte = glulx_mem:get_byte(Memory, StrAddress),
    case TypeByte of
	16#e0    ->
	    io:fwrite("UNCOMPRESSED LATIN1-STRING~n"),
	    MachineState1 = MachineState0;
	16#e1    ->
	    %io:fwrite("COMPRESSED STRING~n"),
	    {_, MachineState1} = glk:glk_put_string(
				   glulx_vm, MachineState0,
				   [decompress(Memory, StrAddress + 1)]);
	16#e2    ->
	    io:fwrite("UNCOMPRESSED UNICODE STRING~n"),
	    MachineState1 = MachineState0;
	_Default ->    
	    io:format("UNKNOWN STRING TYPE: ~4.16.0B~n", [TypeByte]),
	    MachineState1 = MachineState0
    end,
    MachineState1.

%% Sends a whole string to the current Glk stream, optimization to avoid
% Setup Huffman decoding
decompress(Memory, Address) ->
    Header = glulx_mem:header(Memory),
    DecodeTable = Header#glulx_header.decode_table,
    RootNodeAddr = glulx_mem:get_word32(Memory, DecodeTable + 8),
    Rev = decompress(Memory, RootNodeAddr, RootNodeAddr, Address, 0, []),
    %io:format("DECODE TABLE ADDRESS: $~8.16.0B, "
	%      "ROOT NODE ADDR: $~8.16.0B, String: ~p~n",
	%      [DecodeTable, RootNodeAddr,
	%       lists:reverse(Rev)]),
    lists:reverse(Rev).

% recursive Huffman decoder
decompress(Memory, RootNodeAddr, CurrentNodeAddr, CurrentByteAddr,
	   BitNum, String) ->
    %ByteDebug = glulx_mem:get_byte(Memory, CurrentByteAddr),
    %BitDebug = glulx_mem:get_bit(Memory, CurrentByteAddr, BitNum),
    NodeType = glulx_mem:get_byte(Memory, CurrentNodeAddr),
    %io:format("DECOMPRESS NODE: $~8.16.0B, Type: ~w, ADDRESS: $~8.16.0B, "
	%      "BitIndex: ~w, Byte: ~16.2.0B, Bit: ~w~n",
    	 %     [CurrentNodeAddr, NodeType, CurrentByteAddr, BitNum, ByteDebug, BitDebug]),
    case BitNum of
	7        ->
	    NextByteAddr = CurrentByteAddr + 1,
	    NextBitNum = 0;
	_ ->
	    NextByteAddr = CurrentByteAddr,
	    NextBitNum = BitNum + 1
    end,
    case NodeType of
	0 ->
	    % Branch
	    %io:fwrite("CURRENT NODE is a branch~n"),
	    Bit = glulx_mem:get_bit(Memory, CurrentByteAddr, BitNum),
	    NextNodeAddr = glulx_mem:get_word32(Memory, CurrentNodeAddr + 1 +
						4 * Bit),
	    decompress(Memory, RootNodeAddr, NextNodeAddr, NextByteAddr,
		       NextBitNum, String);
	1 ->
	    % String terminator
	    %io:fwrite("CURRENT NODE is a string terminator~n"),
	    String;
	2 ->
	    % Single character
	    %io:fwrite("CURRENT NODE is a single character~n"),
	    Char = glulx_mem:get_byte(Memory, CurrentNodeAddr + 1),
	    decompress(Memory, RootNodeAddr, RootNodeAddr, CurrentByteAddr,
		       BitNum, [Char|String]);
	3 ->
	    % C-style string
	    NewString = append_cstring(String, Memory, CurrentNodeAddr + 1),
	    %io:fwrite("CURRENT NODE is a C style string: ~p~n",
		%     [lists:reverse(NewString)]),
	    decompress(Memory, RootNodeAddr, RootNodeAddr, CurrentByteAddr,
		       BitNum, NewString);
	4 ->
	    % Single unicode character
	    io:fwrite("CURRENT NODE is a unicode character~n");
	5 ->
	    % C-style unicode string
	    io:fwrite("CURRENT NODE is a C style unicode string~n");
	8 ->
	    % indirect reference
	    io:fwrite("CURRENT NODE is an indirect reference~n");
	_ ->
	    io:format("CURRENT NODE is an unknown type: ~w~n", [NodeType])
    end.

append_cstring(String, Memory, Address) -> 
    Char = glulx_mem:get_byte(Memory, Address),
    if
	Char =:= 0 -> String;
	true -> append_cstring([Char | String], Memory, Address + 1)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% GLK Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_word32(#glulx_vm{memory = Memory} = MachineState0, Address, Value) ->
    MachineState0#glulx_vm{
      memory = glulx_mem:set_word32(Memory, Address, Value)}.

set_byte(#glulx_vm{memory = Memory} = MachineState0, Address, Value) ->
    MachineState0#glulx_vm{
      memory = glulx_mem:set_byte(Memory, Address, Value)}.
