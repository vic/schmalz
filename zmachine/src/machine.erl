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
-behaviour(gen_server).
-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).
-include("include/zmachine.hrl").
-record(machine_state, {memory, value_stack, call_stack, pc, streams,
			stream_objs, status}).
-record(routine, {start_address, return_address, invocation_sp, local_vars,
		  return_variable}).
-define(STACK, 0).
-define(is_local_variable(VarNum), VarNum =< 16#0f).
-define(global_var_address(Memory, VarNum),
	memory:global_var_address(Memory) + (VarNum - 16#10) * 2).
-define(trunc16(Value), Value band 2#1111111111111111).

%% Starts the machine as a gen_server.
%% @spec start_link(atom(), memory(), int()) -> Result
start_link(Name, Memory, Timeout) ->
    gen_server:start_link({local, Name}, ?MODULE, [Memory, Timeout], []).

% creates a new MachineState object from the specified Memory object
init([Memory0, Timeout]) ->
    VmState0 = #machine_state{memory = Memory0, value_stack = [],
			      call_stack = [],
			      pc = memory:initial_pc(Memory0),
			      streams = streams:create(),
			      stream_objs = #stream_objs{
				screen = screen:create(80),
				memory = []},
			      status = run},
    %% do file header stuff here for now
    Version = memory:version(Memory0),
    if
	Version =:= 4 ->
	    Flags1 = memory:get_byte(Memory0, 16#01),
	    Memory1 = memory:set_byte(Memory0, 16#20, 40),
	    Memory2 = memory:set_byte(Memory1, 16#21, 80),
	    Memory3 = memory:set_byte(Memory2, 16#01, Flags1 bor 2#00111100),
	    VmState1 = VmState0#machine_state{memory = Memory3};
	Version > 5   ->
	    Flags1 = memory:get_byte(Memory0, 16#01),
	    Memory1 = memory:set_word16(Memory0, 16#22, 80),
	    Memory2 = memory:set_word16(Memory1, 16#24, 40),
	    Memory3 = memory:set_byte(Memory2, 16#26, 1),
	    Memory4 = memory:set_byte(Memory3, 16#27, 1),
	    Memory5 = memory:set_byte(Memory4, 16#01, Flags1 bor 2#00111100),
	    VmState1 = VmState0#machine_state{memory = Memory5};
	true ->
	    VmState1 = VmState0
    end,
    {ok, VmState1, Timeout}.

handle_cast(_Msg, State)            -> {noreply, State}.
handle_info(timeout, State)         -> {stop, "Session timeout !", State};
handle_info(_Info, State)           -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


handle_call(Request, _,
	    #machine_state{pc = PC, memory = Memory, status = Status}
	    = VmState0) ->
    case Request of
	status  -> {reply, Status, VmState0};
	version -> {reply, memory:version(Memory), VmState0};
	pc      -> {reply, PC, VmState0};
	{set_status, NewStatus} ->
	    {reply, ok, VmState0#machine_state{status = NewStatus}};
	{call_routine, PackedAddress, ReturnAddress, Arguments, ReturnVar} ->
	    VmState1 = call_routine(VmState0, PackedAddress,
				    ReturnAddress, Arguments, ReturnVar),
	    {reply, ok, VmState1};
	get_screen ->
	    {Screen, VmState1} = get_screen(VmState0),
	    {reply, Screen, VmState1};
	{set_pc, NewPC} -> {reply, ok, set_pc(VmState0, NewPC)};
	{increment_pc, Increment} ->
	    {reply, ok, set_pc(VmState0, (PC + Increment))};
	update_status_line -> {reply, ok, update_status_line(VmState0)};
        % Memory access
	{get_byte, Address} ->
	    {reply, memory:get_byte(Memory, Address), VmState0};
	{set_byte, Address, Value} ->
	    VmState1 = VmState0#machine_state{
			 memory = memory:set_byte(Memory, Address, Value)},
	    {reply, ok, VmState1};
	{get_word16, Address} ->
	    {reply, memory:get_word16(Memory, Address), VmState0};

	{set_word16, Address, Value} ->
	    VmState1 = VmState0#machine_state{
			 memory = memory:set_word16(Memory, Address, Value)},
	    {reply, ok, VmState1};
	{get_var, VarNum} ->
	    {Value, VmState1} = get_var(VmState0, VarNum),
	    {reply, Value, VmState1};
	{set_var, VarNum, Value} ->
	    VmState1 = set_var(VmState0, VarNum, Value),
	    {reply, ok, VmState1};
	{return_from_routine, ReturnValue} ->
	    VmState1 = return_from_routine(VmState0, ReturnValue),
	    {reply, ok, VmState1};
	{decode_address, Address, MaxAddress} ->
	    {reply, encoding:decode_address(Memory, Address, MaxAddress),
	     VmState0};
	{num_zencoded_bytes, Address} ->
	    {reply, encoding:num_zencoded_bytes(Memory, Address), VmState0};
	{print_zscii, ZsciiString} ->
	    {reply, ok, print_zscii(VmState0, ZsciiString)};
	{print_addr, ByteAddress} ->
	    VmState1 = print_zscii(VmState0,
				   encoding:decode_address(Memory, ByteAddress,
							   undef)),
	    {reply, ok, VmState1};
	{print_paddr, PackedAddress} ->
	    VmState1 = print_zscii(VmState0,
				   decode_packed_address(Memory,
							 PackedAddress)),
	    {reply, ok, VmState1};
	% Objects
	{print_object, ObjectNum} ->
	    VmState1 = print_zscii(VmState0, objects:name(Memory, ObjectNum)),
	    {reply, ok, VmState1};
	{object_parent, Object} ->
	    {reply, objects:parent(Memory, Object), VmState0};
	{object_child, Object} ->
	    {reply, objects:child(Memory, Object), VmState0};
	{object_sibling, Object} ->
	    {reply, objects:sibling(Memory, Object), VmState0};
	{insert_object, Object, Destination} ->
	    {reply, ok, insert_object(VmState0, Object, Destination)};
	{remove_object, Object} ->
	    {reply, ok, remove_object(VmState0, Object)};
	{object_has_attribute, Object, Attribute} ->
	    {reply, objects:has_attribute(Memory, Object, Attribute), VmState0};
	{object_set_attribute, Object, Attribute} ->
	    {reply, ok, object_set_attribute(VmState0, Object, Attribute)};
	{object_clear_attribute, Object, Attribute} ->
	    {reply, ok, object_clear_attribute(VmState0, Object, Attribute)};
	{object_property, Object, Property} ->
	    {reply, objects:property(Memory, Object, Property), VmState0};
	{object_set_property, Object, Property, Value} ->
	    {reply, ok, object_set_property(VmState0, Object, Property, Value)};
	{object_prop_addr, Object, Property} ->
	    {reply, objects:property_address(Memory, Object, Property),
	     VmState0};
	{object_prop_len, PropertyDataAddress} ->
	    {reply, objects:property_length(Memory, PropertyDataAddress),
	     VmState0};
	{object_next_property, Object, Property} ->
	    {reply, objects:next_property_num(Memory, Object, Property),
	     VmState0};
	% I/O
	{output_stream, StreamNum} ->
	    {reply, ok, output_stream(VmState0, StreamNum)};
	{output_stream, StreamNum, TableAddress} ->
	    {reply, ok, output_stream(VmState0, StreamNum, TableAddress)};
	{send_input, InputString} ->
	    {reply, ok, append_input(VmState0, InputString)};
	{sread, TextBuffer, ParseBuffer, StoreVar} ->
	    {reply, ok, sread(VmState0, TextBuffer, ParseBuffer, StoreVar)};
	{read_char, StoreVar} -> {reply, ok, read_char(VmState0, StoreVar)};
	% Window operations
	{erase_window, WindowNum} ->
	    {reply, ok, erase_window(VmState0, WindowNum)};
	{split_window, Lines} ->
	    {reply, ok, split_window(VmState0, Lines)};
	{set_cursor, Line, Column} ->
	    {reply, ok, set_cursor(VmState0, Line, Column)};
	{set_text_style, StyleFlags} ->
	    {reply, ok, set_text_style(VmState0, StyleFlags)};
	{set_window, WindowNum} ->
	    {reply, ok, set_window(VmState0, WindowNum)};
	% Other
	{scan_table, X, Table, Len}       ->
	    {reply, scan_table(Memory, X, Table, Len), VmState0};
	{scan_table, X, Table, Len, Form} ->
	    {reply, scan_table(Memory, X, Table, Len, Form), VmState0};
	% Default
	Other ->
	    io:format("Error in VM request: ~p~n", [Other]),
	    {reply, {error, Other}, VmState0}
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
						?trunc16(Value)))};
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
    Version = memory:version(Memory),
    Address = memory:unpack_address(Memory, PackedAddress),
    NumLocals = memory:get_byte(Memory, Address),
    NumArguments = length(Arguments),
    if
	Version < 5 ->
	    Locals = Arguments ++ get_locals(Memory,
					     Address + 1 + 2 * NumArguments,
					     NumLocals - NumArguments),
	    StartAddress = Address + 1 + NumLocals * 2;
	true ->
	    NumEmptyLocals = util:max(0, NumLocals - NumArguments),
	    Locals = Arguments ++ lists:duplicate(NumEmptyLocals, 0),
	    StartAddress = Address + 1
    end,
    #routine{start_address = StartAddress,
	     local_vars = Locals, invocation_sp = InvocationSP,
	     return_address = ReturnAddress, return_variable = ReturnVariable}.

% return the nth local variable (starting at 0) from the specified
% routine object
routine_get_local(#routine{local_vars = Locals}, Index) ->
    lists:nth(Index + 1, Locals).

% sets the nth local variable to the specified value
routine_set_local(#routine{local_vars = Locals} = Routine, Index, Value) ->
    Routine#routine{local_vars = util:list_replace(Locals, Index + 1, Value)}.

%%%-----------------------------------------------------------------------
%%%  Routine Helpers
%%%-----------------------------------------------------------------------

% Extracts the list of local variables at the specified Address
get_locals(_Memory, _Address, 0)       -> [];
get_locals(Memory, Address, NumLocals) ->
    [ memory:get_word16(Memory, Address) | get_locals(Memory, Address + 2,
						      NumLocals - 1) ].

%%%-----------------------------------------------------------------------
%%% Stack implementation
%%% Originally having its own module, the stack implementation is part
%%% of the machine state implementation because it is not directlyused
%%% outside of it
%%%-----------------------------------------------------------------------

%% @spec stack_push([L], any()) -> [L]
stack_push(Stack, Value) -> [Value | Stack].

%% @spec stack_pop([L]) -> [L]
stack_pop([])          -> undef;
stack_pop([_ | Stack]) -> Stack.

%% @spec stack_top([L]) -> any()
stack_top([])          -> undef;
stack_top([Value | _]) -> Value.

%% @spec stack_size([L]) -> (int())
stack_size(Stack) -> length(Stack).

%% @spec stack_pop_to_sp([L], int()) -> ([L])
stack_pop_to_sp(Stack, SP) -> lists:nthtail(length(Stack) - SP, Stack).

%%%-----------------------------------------------------------------------
%%% Parsing
%%%-----------------------------------------------------------------------

sread(MachineState0, TextBuffer, ParseBuffer, StoreVar) ->
    {InputString, #machine_state{memory = Memory0} = MachineState1} =
	read_input(MachineState0),
    Version = memory:version(Memory0),
    % 1. read the input into the text buffer
    % TextLength = ?call_machine({get_byte, TextBuffer}),
    Memory1 = memory:copy_string_to_address(
		Memory0, TextBuffer + 1,
		string:to_lower(string:strip(InputString))),
    % 2. tokenize the input and store results to parse buffer
    %ParseBufferLength = memory:get_byte(Memory1, ParseBuffer),
    Tokens = dictionary:tokenize_and_lookup(Memory1, InputString),
    %io:format("Tokens: ~p~n", [Tokens]),
    % store number of tokens
    Memory2 = memory:set_byte(Memory1, ParseBuffer + 1, length(Tokens)),
    % store token information
    Memory3 = store_tokens(Memory2, ParseBuffer + 2, Tokens),
    MachineState2 = MachineState1#machine_state{memory = Memory3},
    if
	Version >= 5 ->
	    % TODO: Return Carriage return for now
	    set_var(MachineState2, StoreVar, ?CR);
	true         ->
	    MachineState2
    end.

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

print_zscii(#machine_state{streams = Streams0, stream_objs = StreamObjs0}
	    = VmState0, ZsciiString) ->
    VmState0#machine_state{stream_objs =
			   streams:print_zscii(Streams0, StreamObjs0,
					       ZsciiString)}.

get_screen(#machine_state{stream_objs = #stream_objs{screen = Screen0}}
	   = VmState0) ->
    Version = memory:version(VmState0#machine_state.memory),
    if
	Version =:= 3 ->
	    {ScreenBuffer, Screen1} = screen:get_screen3(Screen0);
	true          ->
	    {ScreenBuffer, Screen1} = screen:get_screen(Screen0)
    end,
    {ScreenBuffer, VmState0#machine_state{
		     stream_objs = #stream_objs{screen = Screen1}}}.

update_status_line(#machine_state{memory = Memory,
				  stream_objs = #stream_objs{screen = Screen0}
				 = StreamObjs0}
		  = VmState0) ->
    VmState0#machine_state{
      stream_objs = StreamObjs0#stream_objs{
	screen = screen:set_status_line(Screen0,
					objects:name(Memory,
						     get_global_var(Memory,
								    1)),
					get_global_var(Memory, 2),
					get_global_var(Memory, 3))}}.

append_input(#machine_state{streams = Streams} = VmState0, InputString) ->
    VmState0#machine_state{streams = streams:append_input(Streams,
							  InputString)}.

read_char(VmState0, StoreVar) ->
    {[Char | _], VmState1} = read_input(VmState0),
    set_var(VmState1, StoreVar, Char band 16#ff).

read_input(#machine_state{streams = Streams0} = VmState0) ->
    {InputString, Streams1} = streams:read_input(Streams0),
    {InputString, VmState0#machine_state{streams = Streams1}}.

erase_window(#machine_state{stream_objs = #stream_objs{screen = Screen0}
			    = StreamObjs0} = VmState0, WindowNum) ->
    VmState0#machine_state{stream_objs =
			   StreamObjs0#stream_objs{
			     screen = screen:erase_window(Screen0, WindowNum)}}.

split_window(#machine_state{stream_objs = #stream_objs{screen = Screen0}
			    = StreamObjs0} = VmState0, Lines) ->
    VmState0#machine_state{stream_objs =
			   StreamObjs0#stream_objs{
			     screen = screen:split_window(Screen0, Lines)}}.

set_cursor(#machine_state{stream_objs =
			  #stream_objs{screen = Screen0} = StreamObjs0}
	   = VmState0, Line, Column) ->
    VmState0#machine_state{stream_objs =
			   StreamObjs0#stream_objs{
			     screen = screen:set_cursor(Screen0, Line,
							Column)}}.

set_window(#machine_state{stream_objs =
			  #stream_objs{screen = Screen0} = StreamObjs0}
	   = VmState0, WindowNum) ->
    VmState0#machine_state{stream_objs =
			   StreamObjs0#stream_objs{
			     screen = screen:set_window(Screen0, WindowNum)}}.

set_text_style(#machine_state{stream_objs =
			      #stream_objs{screen = Screen0} = StreamObjs0}
			      = VmState0, StyleFlags) ->
    VmState0#machine_state{stream_objs =
			   StreamObjs0#stream_objs{
			     screen = screen:set_text_style(Screen0,
							    StyleFlags)}}.


%%%-----------------------------------------------------------------------
%%% Writing memory tables
%%%-----------------------------------------------------------------------

output_stream(#machine_state{memory = Memory0, streams = Streams,
			     stream_objs = #stream_objs{
			       memory = [MemStream|MemStreams]} = StreamObjs0}
	      = VmState0, -3) ->
    #mem_stream{table_address = TableAddress, num_chars = NumChars,
		buffer = Buffer} = MemStream,
    Memory1 = write_table(Memory0, TableAddress, NumChars, Buffer),
    VmState0#machine_state{memory = Memory1,
			   stream_objs = StreamObjs0#stream_objs{
					  memory = MemStreams},
			   streams = streams:output_stream(Streams, -3)};
output_stream(#machine_state{streams = Streams} = VmState0, StreamNum) ->
    VmState0#machine_state{streams = streams:output_stream(Streams, StreamNum)}.

output_stream(#machine_state{streams = Streams,
			     stream_objs = #stream_objs{memory = MemStreams}
			     = StreamObjs0} = VmState0,
	      3, TableAddress) ->
    MemStream = streams:create_mem_stream(TableAddress),
    VmState0#machine_state{streams = streams:output_stream(Streams, 3),
			   stream_objs = StreamObjs0#stream_objs{
					   memory = [MemStream | MemStreams]}}.

write_table(Memory0, TableAddress, NumChars, Buffer) ->
    write_table_data(memory:set_word16(Memory0, TableAddress, NumChars),
		     TableAddress + 2, Buffer).

write_table_data(Memory0, _, []) -> Memory0;
write_table_data(Memory0, Address, [Byte | Bytes]) ->
    write_table_data(memory:set_byte(Memory0, Address, Byte band 16#ff),
		     Address + 1, Bytes).

scan_table(_, _, _, 0) -> 0;
scan_table(Memory, X, Address, Len) ->
    Word = memory:get_word16(Memory, Address),
    if
	Word =:= X -> Address;
	true       -> scan_table(Memory, X, Address + 2, Len - 1)
    end.

scan_table(_Memory0, _X, _Table, _Len, _Form) ->
    undef.
