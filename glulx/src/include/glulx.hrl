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
%%% Description of module glulx.hrl
%%%-----------------------------------------------------------------------
%%%
%%%-----------------------------------------------------------------------

-record(glulx_header, {magic_number, version, ram_start, ext_start, end_mem, 
		       stack_size, start_func, decode_table, checksum}).

-define(CALL, 16#30).

-record(instr, {opcode, operands, address, length}).

-define(call_machine(Message), glulx_vm:rpc(MachinePid, Message)).
-define(get_byte(Address), glulx_vm:rpc(MachinePid, {get_byte, Address})).
-define(get_word16(Address), glulx_vm:rpc(MachinePid, {get_word16, Address})).
-define(get_word32(Address), glulx_vm:rpc(MachinePid, {get_word32, Address})).

