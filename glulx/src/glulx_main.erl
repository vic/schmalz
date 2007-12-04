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
%%% Description of module glulx_main
%%%-----------------------------------------------------------------------
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%%
%%%-----------------------------------------------------------------------

-module(glulx_main).
-export([main/0]).
-include("include/glulx.hrl").

-ifdef(DEBUG).
-define(print_instruction(Instruction),
	glulx_instr:print_instr(Instruction, Num)).
-else.
-define(print_instruction(Instruction), void).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Testing configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The main method
main() ->
  {ok, [[StoryFile]]} = init:get_argument(storyfile),
  io:fwrite("Schmalz (Glulx, Story: ~s) Copyright (C) 2007 Wei-ju Wu~n"
	    "This program comes with ABSOLUTELY NO WARRANTY; "
	    "for details type `show w'.~n"
	    "This is free software, and you are welcome to redistribute it~n"
	    "under certain conditions; type `show c' for details.~n~n~n",
	    [StoryFile]),
  start(StoryFile),
	init:stop().

% starts the Glulx-VM with the specified story file
start(Filename) ->
    MachinePid = glulx_vm:create(glulx_mem:read_file(Filename)),
    run(MachinePid, 1).

run(MachinePid, Num) ->
    Status = ?call_machine(status),
    if
        Status =:= halt -> halt;
        true ->
            Instruction = glulx_decode_instr:decode(MachinePid),
            ?print_instruction(Instruction),
            glulx_instr:execute(MachinePid, Instruction),
	    run(MachinePid, Num + 1)
    end.
