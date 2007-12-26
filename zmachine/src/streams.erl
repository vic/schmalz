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
%%% Description of module streams
%%%-----------------------------------------------------------------------
%%% This module implements the input and output system of Erlkoenig
%%% Z-machine. Currently, only the screen and memory output streams are
%%% supported and the only suported input stream is the keyboard input
%%% stream
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%%
%%%-----------------------------------------------------------------------

-module(streams).
-export([create/0, print_zscii/3, append_input/2, read_input/1]).
-include("include/zscii.hrl").
-record(streams, {current_out, keyboard_in, current_in}).

create() -> #streams{current_out = screen, keyboard_in = [],
		     current_in = keyboard}.

print_zscii(#streams{current_out = screen},
	    Screen, ZsciiString) ->
    screen:print_zscii(Screen, ZsciiString).

append_input(#streams{keyboard_in = KeyboardStream} = Streams,
	     InputString) ->
    Streams#streams{keyboard_in = KeyboardStream ++ InputString}.

read_input(#streams{keyboard_in = KeyboardStream} = Streams) ->
    {KeyboardStream, Streams#streams{keyboard_in = []}}.

