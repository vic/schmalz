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

-define(ADD,          16#10).
-define(ALOAD,        16#48).
-define(ALOADB,       16#4a).
-define(ALOADBIT,     16#4b).
-define(ALOADS,       16#49).
-define(ASTORE,       16#4c).
-define(ASTOREBIT,    16#4f).
-define(BINARYSEARCH, 16#151).
-define(BITAND,       16#18).
-define(CALL,         16#30).
-define(CALLF,        16#160).
-define(CALLFI,       16#161).
-define(CALLFII,      16#162).
-define(CALLFIII,     16#163).
-define(COPY,         16#40).
-define(COPYB,        16#42).
-define(GESTALT,      16#100).
-define(GETMEMSIZE,   16#102).
-define(GLK,          16#130).
-define(JEQ,          16#24).
-define(JGE,          16#27).
-define(JGEU,         16#2b).
-define(JGT,          16#28).
-define(JLT,          16#26).
-define(JNE,          16#25).
-define(JNZ,          16#23).
-define(JUMP,         16#20).
-define(JZ,           16#22).
-define(MUL,          16#12).
-define(NOP,          16#00).
-define(RETURN,       16#31).
-define(SETIOSYS,     16#149).
-define(SUB,          16#11).
-define(STKCOPY,      16#54).
-define(STREAMCHAR,   16#70).
-define(STREAMNUM,    16#71).
-define(STREAMSTR,    16#72).

-record(instr, {opcode, operands, address, opnum_len, length}).

-define(call_machine(Message), glulx_vm:rpc(MachinePid, Message)).
-define(get_byte(Address), glulx_vm:rpc(MachinePid, {get_byte, Address})).
-define(get_word16(Address), glulx_vm:rpc(MachinePid, {get_word16, Address})).
-define(get_word32(Address), glulx_vm:rpc(MachinePid, {get_word32, Address})).
-define(get_signed_byte(Address), glulx_util:unsigned_to_signed8(
				    glulx_vm:rpc(MachinePid,
						 {get_byte, Address}))).
-define(get_signed_word16(Address), glulx_util:unsigned_to_signed16(
				      glulx_vm:rpc(MachinePid,
						   {get_word16, Address}))).
-define(pop(), glulx_vm:rpc(MachinePid, pop)).

