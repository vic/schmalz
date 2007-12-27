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
-export([create/0, print_zscii/3, append_input/2, read_input/1,
	 output_stream/2, create_mem_stream/1]).
-include("include/zmachine.hrl").
-record(streams, {screen_out, memory_out = off, keyboard_in, current_in}).

create() -> #streams{screen_out = true, keyboard_in = [],
		     current_in = keyboard}.

create_mem_stream(TableAddress) ->
    #mem_stream{table_address = TableAddress, num_chars = 0, buffer = []}.

output_stream(Streams0, 0)  -> Streams0;
output_stream(Streams0, 1)  -> Streams0#streams{screen_out = true};
output_stream(Streams0, -1) -> Streams0#streams{screen_out = false};
output_stream(Streams0, 3)  -> Streams0#streams{memory_out = true};
output_stream(Streams0, -3) -> Streams0#streams{memory_out = false}.
    
print_zscii(#streams{screen_out = ScreenSelected, memory_out = MemorySelected},
	    #stream_objs{screen = Screen0, memory = MemStreams0} = StreamObjs0,
	    ZsciiString) ->
    if
	ScreenSelected -> Screen1 = screen:print_zscii(Screen0, ZsciiString);
	true -> Screen1 = Screen0
    end,
    if
	MemorySelected ->
	    [#mem_stream{buffer = Buffer, num_chars = NumChars} = MemStream0 |
	     MemStreamList ] = MemStreams0,
	    MemStreams1 = [MemStream0#mem_stream{
			     buffer = Buffer ++ ZsciiString,
			     num_chars = NumChars + length(ZsciiString)} |
			   MemStreamList];
       true ->
	    MemStreams1 = MemStreams0
    end,
    StreamObjs0#stream_objs{screen = Screen1, memory = MemStreams1}.

append_input(#streams{keyboard_in = KeyboardStream} = Streams,
	     InputString) ->
    Streams#streams{keyboard_in = KeyboardStream ++ InputString}.

read_input(#streams{keyboard_in = KeyboardStream} = Streams) ->
    {KeyboardStream, Streams#streams{keyboard_in = []}}.
