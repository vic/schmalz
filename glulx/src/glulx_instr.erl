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
%%% Description of module glulx_instr
%%%-----------------------------------------------------------------------
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%%
%%%-----------------------------------------------------------------------

-module(glulx_instr).
-export([execute/2, print_instr/2]).
-include("include/glulx.hrl").

execute(MachinePid, #instr{opcode = OpcodeNum, operands = Operands,
			   length = Length}) ->
   case OpcodeNum of
       ?CALL ->
	   ?call_machine({set_status, halt});
       _Default ->
	   undef
   end.
    
print_instr(#instr{address = Address, operands = Operands}, Num) ->
    io:format("~p - $~8.16.0B call ~p~n", [Num, Address, Operands]).
