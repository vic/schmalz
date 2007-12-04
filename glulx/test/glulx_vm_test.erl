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
%%% Description of module glulx_vm_test.
%%%-----------------------------------------------------------------------
%%% This module contains the test suite for glulx_vm.
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%% test_all()
%%%   runs the test suite
%%%-----------------------------------------------------------------------

-module(glulx_vm_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/include/glulx.hrl").
-export([test_all/0]).


test_all() ->
  test(),
  init:stop().
