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
%%% Header file zmachine.hrl
%%%-----------------------------------------------------------------------
%%% This module contains the symbolic constants for the opcodes
%%% specified in the Z-machine specification
%%%-----------------------------------------------------------------------
% Definitions of the opcode numbers

% 2OP opcodes
-define(JE,             16#01).
-define(JL,             16#02).
-define(JG,             16#03).
-define(DEC_CHK,        16#04).
-define(INC_CHK,        16#05).
-define(JIN,            16#06).
-define(TEST,           16#07).
-define(OR,             16#08).
-define(AND,            16#09).
-define(TEST_ATTR,      16#0a).
-define(SET_ATTR,       16#0b).
-define(CLEAR_ATTR,     16#0c).
-define(STORE,          16#0d).
-define(INSERT_OBJ,     16#0e).
-define(LOADW,          16#0f).
-define(LOADB,          16#10).
-define(GET_PROP,       16#11).
-define(GET_PROP_ADDR,  16#12).
-define(GET_NEXT_PROP,  16#13).
-define(ADD,            16#14).
-define(SUB,            16#15).
-define(MUL,            16#16).
-define(DIV,            16#17).
-define(MOD,            16#18).
-define(CALL_2S,        16#19).
-define(CALL_2N,        16#1a).
-define(SET_COLOUR,     16#1b).
-define(THROW,          16#1c).

% 1OP opcodes
-define(JZ,             16#00).
-define(GET_SIBLING,    16#01).
-define(GET_CHILD,      16#02).
-define(GET_PARENT,     16#03).
-define(GET_PROP_LEN,   16#04).
-define(INC,            16#05).
-define(DEC,            16#06).
-define(PRINT_ADDR,     16#07).
-define(CALL_1S,        16#08).
-define(REMOVE_OBJ,     16#09).
-define(PRINT_OBJ,      16#0a).
-define(RET,            16#0b).
-define(JUMP,           16#0c).
-define(PRINT_PADDR,    16#0d).
-define(LOAD,           16#0e).
-define(NOT,            16#0f).

% 0OP opcodes
-define(RTRUE,	        16#00).
-define(RFALSE,	        16#01).
-define(PRINT,          16#02).
-define(PRINT_RET,      16#03).
-define(NOP,            16#04).
-define(SAVE,           16#05).
-define(RESTORE,        16#06).
-define(RESTART,        16#07).
-define(RET_POPPED,     16#08).
-define(POP,            16#09).
-define(QUIT,           16#0a).
-define(NEW_LINE,       16#0b).
-define(SHOW_STATUS,    16#0c).
-define(VERIFY,         16#0d).
-define(PIRACY,         16#0f).

% VAR opcodes
-define(CALL,           16#00).
-define(STOREW,         16#01).
-define(STOREB,         16#02).
-define(PUT_PROP,       16#03).
-define(SREAD,          16#04).
-define(AREAD,          16#04).
-define(PRINT_CHAR,     16#05).
-define(PRINT_NUM,      16#06).
-define(RANDOM,         16#07).
-define(PUSH,           16#08).
-define(PULL,           16#09).
-define(SPLIT_WINDOW,   16#0a).
-define(SET_WINDOW,     16#0b).
-define(CALL_VS2,       16#0c).
-define(ERASE_WINDOW,   16#0d).
-define(ERASE_LINE,     16#0e).
-define(SET_CURSOR,     16#0f).
-define(GET_CURSOR,     16#10).
-define(SET_TEXT_STYLE, 16#11).
-define(BUFFER_MODE,    16#12).
-define(OUTPUT_STREAM,  16#13).
-define(INPUT_STREAM,   16#14).
-define(SOUND_EFFECT,   16#15).
-define(READ_CHAR,      16#16).
-define(SCAN_TABLE,     16#17).
-define(NOT_V5,         16#18).
-define(CALL_VN,        16#19).
-define(CALL_VN2,       16#1a).
-define(TOKENISE,       16#1b).
-define(ENCODE_TEXT,    16#1c).
-define(COPY_TABLE,     16#1d).
-define(PRINT_TABLE,    16#1e).
-define(CHECK_ARG_COUNT,16#1f).

% ZSCII constants
-define(NEWLINE,        13).
-define(SPACE,          32).
-define(EXCLAIMATION,   33).
-define(DOUBLE_QUOTE,   34).
-define(HASH_MARK,      35).
-define(DOLLAR,         36).
-define(SINGLE_QUOTE,   39).
-define(LEFT_PAREN,     40).
-define(RIGHT_PAREN,    41).
-define(COMMA,          44).
-define(DASH,           45).
-define(DOT,            46).
-define(SLASH,          47).
-define(COLON,          58).
-define(QUESTION_MARK,  63).
-define(BACKSLASH,      92).
-define(UNDERSCORE,     95).

% Records
% A record that holds the supported stream objects
-record(stream_objs, {screen, memory}).
-record(mem_stream, {table_address, num_chars, buffer}).
