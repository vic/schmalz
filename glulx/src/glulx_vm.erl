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
-export([create/1, rpc/2]).
-include("include/glulx.hrl").

-record(call_frame, {type, result_spec, return_address, locals}).
-record(glulx_vm, {memory, call_stack, value_stack, pc, status}).

% Destination types for function calls
-define(DO_NOT_STORE,         0).
-define(STORE_IN_MAIN_MEMORY, 1).
-define(STORE_IN_LOCAL_VAR,   2).
-define(STORE_ON_STACK,       3).

%% Creates an initialized Glulx-VM.
%% @spec create(GlulxMem()) -> pid().
create(Memory) ->
    #glulx_header{start_func = StartFuncAddr} = glulx_mem:header(Memory),
    StartFuncFrame = decode_function(Memory, StartFuncAddr),
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
	{From, status} ->
	    ack(From, Status),
	    listen(MachineState0);
	{From, {set_status, NewStatus}} ->
	    ack(From, ok),
	    listen(MachineState0#glulx_vm{status = NewStatus});
	{From, {op_call, Address, NumParams, ReturnSpec}} ->
	    ack(From, ok),
	    MachineState1 = op_call(Address, NumParams, ReturnSpec),
	    listen(MachineState1);
	{From, {get_byte, Address}} ->
	    ack(From, glulx_mem:get_byte(Memory, Address)),
	    listen(MachineState0);
	{From, {get_word16, Address}} ->
	    ack(From, glulx_mem:get_word16(Memory, Address)),
	    listen(MachineState0);
	{From, {get_word32, Address}} ->
	    ack(From, glulx_mem:get_word32(Memory, Address)),
	    listen(MachineState0);
	{From, Other} ->
	    ack(From, {error, Other}),
	    listen(MachineState0)
    end.

ack(MachinePid, Message) ->
    MachinePid ! {self(), Message}.

op_call(Address, NumParams, ReturnSpec) ->
    undef.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Helper fuctionality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% First part of a function call, decodes the function at the specified
%% address.
%% @spec decode_function(GlulxMem(), int()) -> call_frame().
decode_function(Memory, Address) ->
    Type = glulx_mem:get_byte(Memory, Address),
    Locals = read_locals(Memory, Address + 1),
    #call_frame{type = Type, result_spec = ?DO_NOT_STORE,
		return_address = undef, locals = Locals}.

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
   (length(Locals) + 1) * 2 + 1. 
