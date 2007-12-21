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
-export([create/0, print_zscii/2, set_status_line/4, get_screen/1,
	 append_input/2, read_input/1, erase_window/2, split_window/2,
	 set_window/2]).
-include("include/zscii.hrl").
-record(streams, {screen_out, current_out, keyboard_in, current_in}).

create() -> #streams{screen_out = screen:create(80), current_out = screen,
		     keyboard_in = [], current_in = keyboard}.

erase_window(#streams{screen_out = Screen} = Streams, WindowNum) ->
    Streams#streams{screen_out = screen:erase_window(Screen, WindowNum)}.

set_window(#streams{screen_out = Screen} = Streams, WindowNum) ->
    Streams#streams{screen_out = screen:set_window(Screen, WindowNum)}.

split_window(#streams{screen_out = Screen} = Streams, Lines) ->
    Streams#streams{screen_out = screen:split_window(Screen, Lines)}.

print_zscii(#streams{screen_out = Screen, current_out = screen} = Streams,
	    ZsciiString) ->
    Streams#streams{screen_out = screen:print_zscii(Screen, ZsciiString)}.

append_input(#streams{keyboard_in = KeyboardStream} = Streams,
	     InputString) ->
    Streams#streams{keyboard_in = KeyboardStream ++ InputString}.

read_input(#streams{keyboard_in = KeyboardStream} = Streams) ->
    {KeyboardStream, Streams#streams{keyboard_in = []}}.

get_screen(Streams0) ->
    {ObjectName, Value1, Value2} = status_line(Streams0),
    {WindowBottom, Streams1} = bottom_window(Streams0),
    {ok, WindowBottomStr, _} = regexp:gsub(WindowBottom, "\r", "\n"),
    {io_lib:format("~s ~w-~w~n~s",
		   [ObjectName, Value1, Value2, WindowBottomStr]), Streams1}.


status_line(#streams{screen_out = Screen}) -> screen:status_line(Screen).

set_status_line(#streams{screen_out = Screen} = Streams,
		ObjectName, Value1, Value2) ->
    Streams#streams{screen_out = screen:set_status_line(Screen, ObjectName,
						        Value1, Value2)}. 

bottom_window(#streams{screen_out = Screen0} = Streams) ->
  {WindowBottom, Screen1 } = screen:bottom_window(Screen0),
  {WindowBottom, Streams#streams{screen_out = Screen1}}.
