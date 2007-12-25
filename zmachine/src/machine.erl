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
%%% Description of module machine.
%%%-----------------------------------------------------------------------
%%% This module represents the abstract data type machine state and
%%% its related functionality. The machine state consists of the chunk
%%% of memory that contains the story, the stacks, the current PC and the
%%% streams.
%%% The representation uses two stacks as in ZMPP which is used as
%%% a reference implementation, which makes it easier to traverse
%%% and read values and routines from the stack.
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%% create(Memory)
%%%   creates a new MachineState object
%%%-----------------------------------------------------------------------
-module(machine).
-export([create/1, rpc/2, print/1, print_locals/1, dump_input_buffer/1]).
-record(machine_state, {memory, value_stack, call_stack, pc, streams,
			screen, status}).
-record(routine, {start_address, return_address, invocation_sp, local_vars,
		  return_variable}).
-define(STACK, 0).
-define(is_local_variable(VarNum), VarNum =< 16#0f).
-define(global_var_address(Memory, VarNum),
	memory:global_var_address(Memory) + (VarNum - 16#10) * 2).
-define(trunc16(Value), Value band 2#1111111111111111).

% creates a new MachineState object from the specified Memory object
create(Memory) ->
    MachineState = #machine_state {memory = Memory, value_stack = [],
				   call_stack = [],
				   pc = memory:initial_pc(Memory),
				   streams = streams:create(),
				   screen = screen:create(80),
				   status = run},
    spawn(fun() -> listen(MachineState) end).

rpc(MachinePid, Message) ->
    MachinePid ! {self(), Message},
    receive
	{MachinePid, Response} -> Response
    after 500 ->
        io:format("waiting for ack timed out"),
	halt
    end.

ack(MachinePid, Message) ->
    MachinePid ! {self(), Message}.

listen(#machine_state{pc = PC, memory = Memory, status = Status}
       = MachineState0) ->
    receive
	{From, status} ->
	    ack(From, Status),
	    listen(MachineState0);
	{From, {set_status, NewStatus}} ->
	    ack(From, ok),
	    listen(MachineState0#machine_state{status = NewStatus});
	{From, version} ->
	    ack(From, memory:version(Memory)),
	    listen(MachineState0);
	{From, {call_routine, PackedAddress, ReturnAddress, Arguments,
		ReturnVar}} ->
	    MachineState1 = call_routine(MachineState0, PackedAddress,
					 ReturnAddress, Arguments, ReturnVar),
	    ack(From, ok),
	    listen(MachineState1);
	{From, get_screen} ->
	    {Screen, MachineState1} = get_screen(MachineState0),
	    ack(From, Screen),
	    listen(MachineState1);
	{From, pc} ->
	    ack(From, PC),
	    listen(MachineState0);
	{From, {set_pc, NewPC}} ->
	    ack(From, ok),
	    listen(set_pc(MachineState0, NewPC));
	{From, {increment_pc, Increment}} ->
	    ack(From, ok),
	    listen(set_pc(MachineState0, (PC + Increment)));
	{From, {get_byte, Address}} ->
	    ack(From, memory:get_byte(Memory, Address)),
	    listen(MachineState0);
	{From, {set_byte, Address, Value}} ->
	    MachineState1 = MachineState0#machine_state{
	      memory = memory:set_byte(Memory, Address, Value)},
	    ack(From, ok),
	    listen(MachineState1);
	{From, {get_word16, Address}} ->
	    ack(From, memory:get_word16(Memory, Address)),
	    listen(MachineState0);
	{From, {set_word16, Address, Value}} ->
	    MachineState1 = MachineState0#machine_state{
	      memory = memory:set_word16(Memory, Address, Value)},
	    ack(From, ok),
	    listen(MachineState1);
	{From, {get_var, VarNum}} ->
	    {Value, MachineState1 } = get_var(MachineState0, VarNum),
	    ack(From, Value),
	    listen(MachineState1);
	{From, {set_var, VarNum, Value}} ->
	    MachineState1 = set_var(MachineState0, VarNum, Value),
	    ack(From, ok),
	    listen(MachineState1);
	{From, {return_from_routine, ReturnValue}} ->
	    MachineState1 = return_from_routine(MachineState0, ReturnValue),
	    ack(From, ok),
	    listen(MachineState1);
	{From, update_status_line} ->
	    MachineState1 = update_status_line(MachineState0),
	    ack(From, ok),
	    listen(MachineState1);
	{From, {decode_address, Address, MaxAddress}} ->
	    ack(From, encoding:decode_address(Memory, Address, MaxAddress)),
	    listen(MachineState0);
	{From, {num_zencoded_bytes, Address}} ->
	    ack(From, encoding:num_zencoded_bytes(Memory, Address)),
	    listen(MachineState0);
	{From, {print_zscii, ZsciiString}} ->
	    MachineState1 = print_zscii(MachineState0, ZsciiString),
	    ack(From, ok),
	    listen(MachineState1);
	{From, {print_addr, ByteAddress}} ->
	    MachineState1 = print_zscii(
	        MachineState0, encoding:decode_address(Memory, ByteAddress,
						       undef)),
	    ack(From, ok),
	    listen(MachineState1);
	{From, {print_paddr, PackedAddress}} ->
	    MachineState1 = print_zscii(
	        MachineState0, decode_packed_address(Memory, PackedAddress)),
	    ack(From, ok),
	    listen(MachineState1);
	% Objects
	{From, {print_object, ObjectNum}} ->
	    MachineState1 = print_zscii(
	        MachineState0, objects:name(Memory, ObjectNum)),
	    ack(From, ok),
	    listen(MachineState1);
	{From, {object_parent, Object}} ->
	    ack(From, objects:parent(Memory, Object)),
	    listen(MachineState0);
	{From, {object_child, Object}} ->
	    ack(From, objects:child(Memory, Object)),
	    listen(MachineState0);
	{From, {object_sibling, Object}} ->
	    ack(From, objects:sibling(Memory, Object)),
	    listen(MachineState0);
	{From, {insert_object, Object, Destination}} ->
	    MachineState1 = insert_object(MachineState0, Object, Destination),
	    ack(From, ok),
	    listen(MachineState1);
	{From, {remove_object, Object}} ->
	    MachineState1 = remove_object(MachineState0, Object),
	    ack(From, ok),
	    listen(MachineState1);
	{From, {object_has_attribute, Object, Attribute}} ->
	    ack(From, objects:has_attribute(Memory, Object, Attribute)),
	    listen(MachineState0);
	{From, {object_set_attribute, Object, Attribute}} ->
	    MachineState1 = object_set_attribute(MachineState0, Object,
						 Attribute),
	    ack(From, ok),
	    listen(MachineState1);
	{From, {object_clear_attribute, Object, Attribute}} ->
	    MachineState1 = object_clear_attribute(MachineState0, Object,
						   Attribute),
	    ack(From, ok),
	    listen(MachineState1);
	{From, {object_property, Object, Property}} ->
	    ack(From, objects:property(Memory, Object, Property)),
	    listen(MachineState0);
	{From, {object_set_property, Object, Property, Value}} ->
	    MachineState1 = object_set_property(MachineState0, Object, Property,
						Value),
	    ack(From, ok),
	    listen(MachineState1);
	{From, {object_prop_addr, Object, Property}} ->
	    ack(From, objects:property_address(Memory, Object, Property)),
	    listen(MachineState0);
	{From, {object_prop_len, PropertyDataAddress}} ->
	    ack(From, objects:property_length(Memory, PropertyDataAddress)),
	    listen(MachineState0);
	{From, {object_next_property, Object, Property}} ->
	    ack(From, objects:next_property_num(Memory, Object, Property)),
	    listen(MachineState0);
	% I/O
	{From, {send_input, InputString0}} ->
	    InputString1 = string:to_lower(string:strip(InputString0)),
	    MachineState1 = append_input(MachineState0, InputString1),
	    ack(From, ok),
	    listen(MachineState1);
	{From, {sread, TextBuffer, ParseBuffer}} ->
	    MachineState1 = sread(MachineState0, TextBuffer, ParseBuffer),
	    ack(From, ok),
	    listen(MachineState1);
	% Window operations
	{From, {erase_window, WindowNum}} ->
	    MachineState1 = erase_window(MachineState0, WindowNum),
	    ack(From, ok),
	    listen(MachineState1);
	{From, {split_window, Lines}} ->
	    MachineState1 = split_window(MachineState0, Lines),
	    ack(From, ok),
	    listen(MachineState1);
	{From, {set_text_style, StyleFlags}} ->
	    MachineState1 = set_text_style(MachineState0, StyleFlags),
	    ack(From, ok),
	    listen(MachineState1);
	{From, {set_window, WindowNum}} ->
	    MachineState1 = set_window(MachineState0, WindowNum),
	    ack(From, ok),
	    listen(MachineState1);
	{From,Other} ->
	    ack(From, {error, Other}),
	    listen(MachineState0)
    end.

sp(#machine_state{value_stack = ValueStack}) -> stack_size(ValueStack).

set_pc(MachineState, NewPC) ->
    MachineState#machine_state{pc = NewPC}.

push_routine(#machine_state{call_stack = CallStack} = MachineState, Routine) ->
    MachineState#machine_state{ call_stack = stack_push(CallStack, Routine) }.

pop_routine(#machine_state{call_stack = CallStack} = MachineState) ->
    MachineState#machine_state{ call_stack = stack_pop(CallStack) }.

call_routine(#machine_state{memory = Memory} = MachineState, PackedAddress,
	     ReturnAddress, Arguments, StoreVariable) ->
    #routine{start_address = RoutineStart} = Routine =
	create_routine(Memory, PackedAddress, sp(MachineState), ReturnAddress,
		       Arguments, StoreVariable),
    push_routine(set_pc(MachineState, RoutineStart), Routine).

% return from the current routine, which pops the current routine off the
% call stack, restores the value stack, sets the result and the return address
return_from_routine(#machine_state{call_stack = CallStack} = MachineState,
		    ReturnValue) ->
    #routine{invocation_sp = InvocationSP, return_variable = ReturnVar,
	     return_address = ReturnAddress} = stack_top(CallStack),
    set_pc(set_var(restore_sp(pop_routine(MachineState), InvocationSP),
		   ReturnVar, ReturnValue),
	   ReturnAddress).

% pops the stack to the specified stack pointer
restore_sp(#machine_state{value_stack = ValueStack} = MachineState, SP) ->
    MachineState#machine_state{value_stack = stack_pop_to_sp(ValueStack, SP)}.

% reads the specified variable, 0 is the stack, 1-16 are the local variables
% of the current routine, 17-255 are the global variables.
% Since the stack might be affected by accessing variable 0,
% return the pair of the value and the new machine state
get_var(#machine_state{value_stack = ValueStack,
		       call_stack = CallStack} = MachineState, ?STACK) ->
    check_underflow(ValueStack, stack_top(CallStack)),
    { stack_top(ValueStack),
      MachineState#machine_state{value_stack = stack_pop(ValueStack)} };
get_var(#machine_state{call_stack = CallStack} = MachineState, VarNum)
  when ?is_local_variable(VarNum)                                        ->
    { routine_get_local(stack_top(CallStack), VarNum - 1), MachineState };
get_var(#machine_state{memory = Memory} = MachineState, VarNum)          ->
    { memory:get_word16(Memory, ?global_var_address(Memory, VarNum)),
      MachineState }.

% retrieves the value of the specified global variable, starting from 1
get_global_var(Memory, VarNum) ->
    memory:get_word16(Memory, memory:global_var_address(Memory) +
		      (VarNum - 1) * 2).

check_underflow(ValueStack, #routine{invocation_sp = InvocationSP}) ->
    if
	length(ValueStack) =:= InvocationSP ->
	    io:fwrite("STACK UNDERFLOW !\n");
	true -> undef
    end;
check_underflow(_ValueStack, undef) -> undef.
    

% sets the specified variable in the machine state, as in get_var(),
% this affects either the stack, the local or the global variables,
% depending on the value of VarNum
set_var(MachineState, undef, _Value)                                   ->
    MachineState;
set_var(#machine_state{value_stack = ValueStack} = MachineState, ?STACK,
	Value)                                                         ->
  MachineState#machine_state{ value_stack = stack_push(ValueStack,
						       ?trunc16(Value)) };
set_var(#machine_state{call_stack = CallStack} = MachineState, VarNum, Value)
  when ?is_local_variable(VarNum)                                      ->
    Routine = stack_top(CallStack),
    MachineState#machine_state{
      call_stack = stack_push(stack_pop(CallStack),
			      routine_set_local(Routine,
						VarNum - 1,
						?trunc16(Value))) };
set_var(#machine_state{memory = Memory} = MachineState, VarNum, Value) ->
    MachineState#machine_state{ memory =
				memory:set_word16(Memory,
						  ?global_var_address(Memory,
								      VarNum),
						  ?trunc16(Value)) }.

insert_object(#machine_state{memory = Memory} = MachineState, Object,
	      Destination) ->
    MachineState#machine_state{
      memory = objects:insert_object(Memory, Object, Destination)}.

remove_object(#machine_state{memory = Memory} = MachineState, Object) ->
    MachineState#machine_state{
      memory = objects:remove_object(Memory, Object)}.

object_set_attribute(#machine_state{memory = Memory} = MachineState, Object,
		     Attribute) ->
    MachineState#machine_state{memory = objects:set_attribute(Memory, Object,
							      Attribute)}.

object_clear_attribute(#machine_state{memory = Memory} = MachineState, Object,
		       Attribute) ->
    MachineState#machine_state{memory = objects:clear_attribute(Memory, Object,
								Attribute)}.

object_set_property(#machine_state{memory = Memory} = MachineState, Object,
		    Property, Value) ->
    MachineState#machine_state{memory = objects:set_property(Memory, Object,
							     Property, Value)}.

decode_packed_address(Memory, PackedAddress) ->
    encoding:decode_address(Memory,
			    memory:unpack_address(Memory, PackedAddress),
			    undef).

%%%-----------------------------------------------------------------------
%%% Routine implementation
%%% Routines are closely related to the machine state, so it makes
%%% sense to place the functions in the machine module
%%%-----------------------------------------------------------------------

% decodes the routine at the specified packed address
create_routine(Memory, PackedAddress, InvocationSP, ReturnAddress, Arguments,
	       ReturnVariable) -> 
    Address = memory:unpack_address(Memory, PackedAddress),
    NumLocals = memory:get_byte(Memory, Address),
    NumArguments = length(Arguments),
    Locals = Arguments ++ get_locals(Memory, Address + 1 + 2 * NumArguments,
				     NumLocals - NumArguments),  
    #routine{start_address = (Address + 1 +  NumLocals * 2),
	     local_vars = Locals, invocation_sp = InvocationSP,
	     return_address = ReturnAddress, return_variable = ReturnVariable}.

% return the nth local variable (starting at 0) from the specified
% routine object
routine_get_local(#routine{local_vars = Locals}, Index) ->
    lists:nth(Index + 1, Locals).

% sets the nth local variable to the specified value
routine_set_local(#routine{local_vars = Locals} = Routine, Index, Value) ->
    Routine#routine{ local_vars = set_local_list(Locals, Index, Value) }.

%%%-----------------------------------------------------------------------
%%%  Routine Helpers
%%%-----------------------------------------------------------------------

% Extracts the list of local variables at the specified Address
get_locals(_Memory, _Address, 0)       -> [];
get_locals(Memory, Address, NumLocals) ->
    [ memory:get_word16(Memory, Address) | get_locals(Memory, Address + 2,
						      NumLocals - 1) ].

% replaces the nth value in the given list with the specified value
set_local_list(Locals, Index, Value) ->
    lists:append([lists:sublist(Locals, Index), [Value],
		  lists:sublist(Locals, Index + 2, length(Locals) - 1)]).

%%%-----------------------------------------------------------------------
%%% Stack implementation
%%% Originally having its own module, the stack implementation is part
%%% of the machine state implementation because it is not directlyused
%%% outside of it
%%%-----------------------------------------------------------------------

%% @spec push([L], any()) -> [L].
stack_push(Stack, Value) -> [Value | Stack].

%% @spec pop([L]) -> [L].
stack_pop([])          -> undef;
stack_pop([_ | Stack]) -> Stack.

%% @spec top([L]) -> any().
stack_top([])          -> undef;
stack_top([Value | _]) -> Value.

%% @spec stack_size([L]) -> (int()).
stack_size(Stack) -> length(Stack).

%% @spec pop_to_sp([L]) -> ([L]).
stack_pop_to_sp(Stack, SP) -> lists:nthtail(length(Stack) - SP, Stack).

%%%-----------------------------------------------------------------------
%%% Parsing
%%%-----------------------------------------------------------------------

sread(MachineState0, TextBuffer, ParseBuffer) ->
    {InputString, #machine_state{memory = Memory0} = MachineState1} =
	read_input(MachineState0),
    % 1. read the input into the text buffer
    % TextLength = ?call_machine({get_byte, TextBuffer}),
    Memory1 = memory:copy_string_to_address(Memory0, TextBuffer + 1,
					    InputString),
    % 2. tokenize the input and store results to parse buffer
    %ParseBufferLength = memory:get_byte(Memory1, ParseBuffer),
    Tokens = dictionary:tokenize_and_lookup(Memory1, InputString),
    %io:format("Tokens: ~p~n", [Tokens]),
    % store number of tokens
    Memory2 = memory:set_byte(Memory1, ParseBuffer + 1, length(Tokens)),
    % store token information
    Memory3 = store_tokens(Memory2, ParseBuffer + 2, Tokens),
    MachineState1#machine_state{memory = Memory3}.

% this could possibly be a fold
store_tokens(Memory0, _Address, []) -> Memory0;
store_tokens(Memory0, Address, [{Pos, WordAddress, String} | Tokens]) ->
    Memory1 = memory:set_word16(Memory0, Address, WordAddress),
    Memory2 = memory:set_byte(Memory1, Address + 2, length(String)),
    Memory3 = memory:set_byte(Memory2, Address + 3, Pos),
    store_tokens(Memory3, Address + 4, Tokens).
    
%%%-----------------------------------------------------------------------
%%% I/O
%%%-----------------------------------------------------------------------

print_zscii(#machine_state{streams = Streams, screen = Screen} = MachineState,
	    ZsciiString) ->
    MachineState#machine_state{
      screen = streams:print_zscii(Streams, Screen, ZsciiString)}.

get_screen(#machine_state{screen = Screen0} = MachineState) ->
    {ScreenBuffer, Screen1} = screen:get_screen(Screen0),
    {ScreenBuffer, MachineState#machine_state{screen = Screen1}}.

update_status_line(#machine_state{memory = Memory, screen = Screen}
		  = MachineState) ->
    MachineState#machine_state{screen = screen:set_status_line(
					   Screen,
					   objects:name(Memory,
							get_global_var(Memory,
								       1)),
					   get_global_var(Memory, 2),
					   get_global_var(Memory, 3))}.

append_input(#machine_state{streams = Streams} = MachineState,
	     InputString) ->
    MachineState#machine_state{streams = streams:append_input(Streams,
							      InputString)}.

read_input(#machine_state{streams = Streams0} = MachineState) ->
    {InputString, Streams1} = streams:read_input(Streams0),
    {InputString, MachineState#machine_state{streams = Streams1}}.

dump_input_buffer(#machine_state{streams = Streams}) ->
    streams:dump_input(Streams).

erase_window(#machine_state{screen = Screen} = VmState0, WindowNum) ->
    VmState0#machine_state{screen = screen:erase_window(Screen, WindowNum)}.

split_window(#machine_state{screen = Screen} = VmState0, Lines) ->
    VmState0#machine_state{screen = screen:split_window(Screen, Lines)}.

set_window(#machine_state{screen = Screen} = VmState0, WindowNum) ->
    VmState0#machine_state{screen = screen:set_window(Screen, WindowNum)}.

set_text_style(#machine_state{screen = Screen} = VmState0, StyleFlags) ->
    VmState0#machine_state{screen = screen:set_text_style(Screen,
							   StyleFlags)}.

%%%-----------------------------------------------------------------------
%%% State printing
%%%-----------------------------------------------------------------------

print(#machine_state{value_stack = ValueStack, call_stack = CallStack}) ->
    io:format("  stack = ~w~n", [ValueStack]),    
    print_locals(stack_top(CallStack)).

print_locals(#routine{local_vars = LocalVars}) ->
    io:format("  locals = ~w~n", [LocalVars]);
print_locals(undef) -> undef.
