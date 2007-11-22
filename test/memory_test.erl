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
%%% Description of module memory_test.
%%%-----------------------------------------------------------------------
%%% This module contains the test suite for memory.
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%% test_all()
%%%   runs the test suite
%%%-----------------------------------------------------------------------

-module(memory_test).
-include_lib("eunit/include/eunit.hrl").
-export([test_all/0]).

minizork_memory_test() ->
  Memory = memory:read_file("../core/testfiles/minizork.z3"),
  ?assertMatch(3, memory:version(Memory)),
  ?assertMatch(1234 * 2, memory:unpack_address(Memory, 1234)),
  ?assertMatch(16#03c6, memory:object_table_address(Memory)),
  ?assertMatch(16#285a, memory:dictionary_address(Memory)),
  ?assertMatch(16#285a, memory:dictionary_address(Memory)),
  ?assertMatch(16#01f4, memory:abbrev_address(Memory)),
  ?assertMatch(16#02b4, memory:global_var_address(Memory)),
  ?assertMatch(16#37d9, memory:initial_pc(Memory)).

copy_string_to_address_test() ->
    Memory = << 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 >>,
    ?assertMatch(<< 1, 1, $H, $i, 0, 1, 1, 1, 1, 1>>,
		 memory:copy_string_to_address(Memory, 2, "Hi")).

get_bytes_test() ->
    Memory = << 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 >>,
    ?assertMatch(<< 3, 4, 5 >>, memory:get_bytes(Memory, 2, 3)).

get_byte_test() ->
    Memory = << 1, 2, 3, 4, 5 >>,
    ?assertMatch(3, memory:get_byte(Memory, 2)).

set_byte_test() ->
    Memory = << 1, 2, 3, 4, 5 >>,
    ?assertMatch(<< 1, 7, 3, 4, 5 >>, memory:set_byte(Memory, 1, 7)).

get_word16_test() ->
    Memory = << 1, 2, 3, 4, 5 >>,
    ?assertMatch(16#0304, memory:get_word16(Memory, 2)).

set_word16_test() ->
    Memory = << 1, 2, 3, 4, 5 >>,
    ?assertMatch(<< 1, 2, 3, 16#ff, 16#aa>>,
		 memory:set_word16(Memory, 3, 16#ffaa)).

test_all() ->
  test(),
  init:stop().
