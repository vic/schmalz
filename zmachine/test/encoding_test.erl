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
%%% Description of module encoding_test.
%%%-----------------------------------------------------------------------
%%% This module contains the test suite for the encoding module.
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%% test_all()
%%%   runs the test suite
%%%-----------------------------------------------------------------------

-module(encoding_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/include/zscii.hrl").
-export([test_all/0]).

decode_address_test() ->
    % without truncation
    MemoryHello = << 0, 0, 2#00010001, 2#10101010, 2#11000110, 2#00110100, 0 >>,
    ?assertMatch("Hello", encoding:decode_address(MemoryHello, 2, undef)),
    
    % with truncation
    MemoryMailBox = << 0, 2#01001000, 2#11001110, 2#01000100, 2#11110100,
		       2#00010001, 2#10101010 >>,
    ?assertMatch("mailbo", encoding:decode_address(MemoryMailBox, 1, 4)).

num__zencoded_bytes_test() ->
    Memory2Bytes = << 2#10010100, 2#10100101, 0, 0 >>,
    Memory4Bytes = << 0, 2#00010100, 2#10100101, 2#10010100, 2#10100101, 0 >>,
    ?assertMatch(2, encoding:num_zencoded_bytes(Memory2Bytes, 0)),
    ?assertMatch(4, encoding:num_zencoded_bytes(Memory4Bytes, 1)).

is_space_test() ->
    ?assert(encoding:is_space(?SPACE)),
    ?assertNot(encoding:is_space($a)).

ord_test() ->
    ?assert(encoding:ord(?DOLLAR) < encoding:ord($a)),
    ?assert(encoding:ord(?DOLLAR) < encoding:ord(?NEWLINE)),
    ?assert(encoding:ord(?NEWLINE) < encoding:ord($a)),
    ?assert(encoding:ord(?NEWLINE) < encoding:ord($0)),
    ?assert(encoding:ord($0) < encoding:ord($a)),
    ?assert(encoding:ord($1) < encoding:ord($a)),
    ?assert(encoding:ord($2) < encoding:ord($a)),
    ?assert(encoding:ord($2) < encoding:ord($a)),
    ?assert(encoding:ord($3) < encoding:ord($a)),
    ?assert(encoding:ord($4) < encoding:ord($a)),
    ?assert(encoding:ord($5) < encoding:ord($a)),
    ?assert(encoding:ord($6) < encoding:ord($a)),
    ?assert(encoding:ord($7) < encoding:ord($a)),
    ?assert(encoding:ord($8) < encoding:ord($a)),
    ?assert(encoding:ord($9) < encoding:ord($a)).

test_all() ->
  test(),
  init:stop().
