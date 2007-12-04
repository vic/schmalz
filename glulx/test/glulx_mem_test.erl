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
%%% Description of module glulx_mem_test.
%%%-----------------------------------------------------------------------
%%% This module contains the test suite for glulx_mem.
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%% test_all()
%%%   runs the test suite
%%%-----------------------------------------------------------------------

-module(glulx_mem_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/include/glulx.hrl").
-export([test_all/0]).

risorg_memory_test() ->
  Memory = glulx_mem:read_file("../testfiles/risorg.ulx"),
  Header = glulx_mem:header(Memory),
  ?assertMatch(16#476c756c, Header#glulx_header.magic_number),
  ?assertMatch({ 2, 0, 0 }, Header#glulx_header.version),
  ?assertMatch(16#0006cb00, Header#glulx_header.ram_start),
  ?assertMatch(16#00085200, Header#glulx_header.ext_start),
  ?assertMatch(16#00085200, Header#glulx_header.end_mem),
  ?assertMatch(4096, Header#glulx_header.stack_size),
  ?assertMatch(16#0000003c, Header#glulx_header.start_func),
  ?assertMatch(16#00034025, Header#glulx_header.decode_table),
  ?assertMatch(16#553eb87e, Header#glulx_header.checksum).

get_byte_base_test() ->
    Memory = {<< 1, 2, 3, 4, 5 >>, <<>>},
    ?assertMatch(3, glulx_mem:get_byte(Memory, 2)).

get_word16_base_test() ->
    Memory = {<< 1, 2, 3, 4, 5 >>, <<>>},
    ?assertMatch(16#0304, glulx_mem:get_word16(Memory, 2)).

get_word32_base_test() ->
    Memory = {<< 1, 2, 3, 4, 5 >>, <<>>},
    ?assertMatch(16#02030405, glulx_mem:get_word32(Memory, 1)).

get_byte_ext_test() ->
    Memory = {<< 1, 2, 3, 4, 5 >>, << 11, 12, 13, 14, 15 >>},
    ?assertMatch(13, glulx_mem:get_byte(Memory, 7)).

get_word16_ext_test() ->
    Memory = {<< 1, 2, 3, 4, 5 >>, << 11, 12, 13, 14, 15>>},
    ?assertMatch(16#0d0e, glulx_mem:get_word16(Memory, 7)).

get_word32_ext_test() ->
    Memory = {<< 1, 2, 3, 4, 5 >>, << 11, 12, 13, 14, 15 >>},
    ?assertMatch(16#0c0d0e0f, glulx_mem:get_word32(Memory, 6)).

%set_byte_test() ->
%    Memory = << 1, 2, 3, 4, 5 >>,
%    ?assertMatch(<< 1, 7, 3, 4, 5 >>, memory:set_byte(Memory, 1, 7)).

%set_word16_test() ->
%    Memory = << 1, 2, 3, 4, 5 >>,
%    ?assertMatch(<< 1, 2, 3, 16#ff, 16#aa>>,
%		 memory:set_word16(Memory, 3, 16#ffaa)).

test_all() ->
  test(),
  init:stop().
