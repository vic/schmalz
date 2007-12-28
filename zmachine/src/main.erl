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
%%% Description of module main
%%%-----------------------------------------------------------------------
%%% This module controls the Z-Machine execution
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%%
%%%-----------------------------------------------------------------------
-module(main).
-export([main/0]).

-ifdef(DEBUG).
-define(print_instruction(Instruction),
	instruction:print_instr(MachinePid, Instruction, Num)).
-else.
-define(print_instruction(Instruction), void).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Testing configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The main method
main() ->
  {ok, [[StoryFile]]} = init:get_argument(storyfile),
  io:fwrite("Schmalz (Z-machine, Story: ~s) Copyright (C) 2007 Wei-ju Wu~n"
	    "This program comes with ABSOLUTELY NO WARRANTY; "
	    "for details type `show w'.~n"
	    "This is free software, and you are welcome to redistribute it~n"
	    "under certain conditions; type `show c' for details.~n~n~n",
	    [StoryFile]),
  start(StoryFile),
	init:stop().

% starts the Z-machine with the specified story file
start(Filename) ->
    MachinePid = machine:create(memory:read_file(Filename)),
    Version = machine:rpc(MachinePid, version),
    run(MachinePid, Version, 0).

run(MachinePid, Version, Num) ->
    Status = machine:rpc(MachinePid, status),
    if
        Status =:= halt -> halt;
	Status =:= sread; Status =:= read_char ->
	    update_status_line(MachinePid, Version),
	    % flush the output buffer
            Output = machine:rpc(MachinePid, get_screen),
            io:fwrite(Output),	    
	    Input = io:get_line(' '),
	    Input2 = string:substr(Input, 1, string:len(Input) - 1),
	    machine:rpc(MachinePid, {send_input, Input2}),
	    % decode and execute again
	    decode_and_execute(MachinePid, Version, Num);
        true ->
	    decode_and_execute(MachinePid, Version, Num + 1)
    end.

update_status_line(MachinePid, Version) when Version =< 3 ->
    machine:rpc(MachinePid, update_status_line);
update_status_line(_, _) -> undef.
    

decode_and_execute(MachinePid, Version, Num) ->
    Instruction = decode_instr:get_instruction(MachinePid),
    ?print_instruction(Instruction),
    instruction:execute(Instruction, MachinePid),
    run(MachinePid, Version, Num).
 
