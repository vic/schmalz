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
-export([create/1, rpc/2, set_local_list/3]).
-include("include/glulx.hrl").

-record(call_frame, {type, result_spec, return_address, invocation_sp,
		     locals}).
-record(glulx_vm, {memory, call_stack, value_stack, pc, status}).

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
		   status = run},
    spawn(fun() -> listen(Vm) end).

rpc(MachinePid, Message) ->
    MachinePid ! {self(), Message},
    receive
	{MachinePid, Response} -> Response
    after 500 ->
        io:format("waiting for ack timed out"),
	halt
    end.

listen(#glulx_vm{memory = Memory, pc = PC, status = Status} = MachineState0) ->
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
	{From, pop} ->
	    {StackValue, MachineState1} = pop(MachineState0),
	    ack(From, StackValue),
	    listen(MachineState1);
	{From, {get_local, Address}} ->
	    ack(From, get_local(MachineState0, Address)),
	    listen(MachineState0);
	{From, {store_value, Value, Dest}} ->
	    MachineState1 = store_value(MachineState0, Value, Dest),
	    %[CallFrame|_] = MachineState1#glulx_vm.call_stack,
	    %io:format("CallFrame after: ~p~n", [CallFrame]),
	    ack(From, ok),
	    listen(MachineState1);
	{From, Other} ->
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
    copy_args(MachineState2, Args).

%% Pops the specicfied number of elements from the stack.
%% @spec pop_stack_values(glulx_vm(), int()) -> {[int()], glulx_vm()}.
pop_stack_values(#glulx_vm{value_stack = ValueStack} = MachineState0,
		 NumValues) ->
    Values = lists:sublist(ValueStack, NumValues),
    if
	length(Values) > 0 ->
	    NewStack = lists:sublist(ValueStack, NumValues + 1,
				     length(ValueStack) - NumValues);
	true ->
	    NewStack = ValueStack
    end,
    {Values, MachineState0#glulx_vm{value_stack = NewStack}}.
    
%% Call with explicit arguments
callf(#glulx_vm{memory = Memory, value_stack = ValueStack} = MachineState0,
      Address, Args, ReturnAddress, ResultSpec) ->
    CallFrame = decode_function(Memory, Address, length(ValueStack),
				ReturnAddress, ResultSpec),
    MachineState1 = push_call_frame(MachineState0, CallFrame, Address),
    MachineState2 = copy_args(MachineState1, Args),
    #glulx_vm{call_stack = [CallFrame2 | _], value_stack = ValueStack2}
	= MachineState2,
    io:format("@callf with CallFrame: ~p Stack: ~p~n",
	      [CallFrame2, ValueStack2]),
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
    NewValueStack = lists:nthtail(length(ValueStack) -
				  CallFrame#call_frame.invocation_sp,
				  ValueStack),
    MachineState1 =
	MachineState0#glulx_vm{pc = ReturnAddress, call_stack = CallStack,
			       value_stack = NewValueStack},
    store_value(MachineState1, Result, CallFrame#call_frame.result_spec).

pop(#glulx_vm{value_stack = [TopValue | Stack]} = MachineState0) ->
    {TopValue, MachineState0#glulx_vm{value_stack = Stack}}.

push(#glulx_vm{value_stack = Stack} = MachineState0, Value) ->
    MachineState0#glulx_vm{value_stack = [Value|Stack]}.

store_value(MachineState0, _Value, {const, 0}) -> MachineState0;
store_value(MachineState0, Value, {stack, _}) ->
    push(MachineState0, Value);
store_value(MachineState0, Value, {local, Address}) ->
    set_local(MachineState0, Address, Value);
store_value(MachineState0, Value, {memory, Address}) ->
    set_word32(MachineState0, Address, Value).

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

set_word32(_, _, _) -> undef.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Helper fuctionality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% First part of a function call, decodes the function at the specified
%% address.
%% @spec decode_function(GlulxMem(), int()) -> call_frame().
decode_function(Memory, Address, InvocationSP, ReturnAddress,
		ResultSpec) ->
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
