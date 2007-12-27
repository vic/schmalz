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
%%% Description of module screen
%%%-----------------------------------------------------------------------
%%% This module implements the screen model of the Z-Machine
%%% The idea behind the screen model is to buffer output between
%%% subsequent runs.
%%% The top window is a list of rows containing pairs of 
%%% {[attribute], [character]}, in order to hold character sequences
%%% with their respective attribute set
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%%
%%%-----------------------------------------------------------------------

-module(screen).
-export([create/1, status_line/1, set_status_line/4, bottom_window/1,
	 print_zscii/2, split_window/2, erase_window/2,
	 set_cursor/3, set_window/2, set_text_style/2, get_screen3/1,
	 get_screen/1]).
-include("include/zmachine.hrl").

-define(STYLE_ROMAN,         0).
-define(STYLE_REVERSE_VIDEO, 1).
-define(STYLE_BOLD,          2).
-define(STYLE_ITALIC,        4).
-define(STYLE_FIXED,         8).

-record(screen, {status_line, window_top, window_bottom, current}).
-record(text_grid, {num_rows, num_cols, buffer, style, cursorx, cursory}).

create(NumColumns) ->
    #screen{status_line = {"(Empty)", 0, 0},
	    window_top = #text_grid{num_rows = 0, num_cols = NumColumns,
				    buffer = [], style = 0, cursorx = 1,
			            cursory = 1},
	    window_bottom = [], current = bottom}.

status_line(#screen{status_line = {ObjectName, Value1, Value2}}) ->
    {ObjectName, Value1, Value2}.

set_status_line(Screen, ObjectName, Value1, Value2) ->
    Screen#screen{status_line = {ObjectName, Value1, Value2}}.

bottom_window(#screen{window_bottom = WindowBottom} = Screen) ->
  {WindowBottom, Screen#screen{window_bottom = []}}.

print_zscii(#screen{current = bottom, window_bottom = Window} = Screen,
	    ZsciiString) ->
    Screen#screen{window_bottom = Window ++ ZsciiString};
print_zscii(#screen{current = top, window_top = TopWindow} = Screen,
	    ZsciiString) ->
    Screen#screen{window_top = print_zscii_top(TopWindow, ZsciiString)}.

print_zscii_top(TopWindow, []) -> TopWindow;
print_zscii_top(#text_grid{cursorx = CursorX} = TopWindow0, [Char | String]) ->
    print_zscii_top(TopWindow0#text_grid{
		      buffer = print_zscii_char_top(TopWindow0, Char),
		      cursorx = CursorX + 1},
		    String).

print_zscii_char_top(#text_grid{buffer = Buffer, style = Style,
				cursory = CursorY, cursorx = CursorX}, Char) ->
    Row0 = lists:nth(CursorY, Buffer),
    Row1 = util:list_replace(Row0, CursorX, {style_string(Style), Char}),
    util:list_replace(Buffer, CursorY, Row1).

split_window(#screen{window_top = #text_grid{num_cols = NumColumns,
					     buffer = Rows} = TopWindow}
	     = Screen, NewNumRows) ->
    Diff = NewNumRows - length(Rows),
    NewRows =
	if
	    Diff =< 0 -> Rows;
	    true -> Rows ++ generate_empty_rows(Diff, NumColumns)
    end,
    Screen#screen{window_top = TopWindow#text_grid{num_rows = NewNumRows,
						   buffer = NewRows}}.

erase_window(#screen{window_top = WindowTop}, -1) ->
    create(WindowTop#text_grid.num_cols).

set_cursor(#screen{current = top, window_top = WindowTop} = Screen,
	   Line, Column) ->
    Screen#screen{window_top = WindowTop#text_grid{cursorx = Column,
						    cursory = Line}}.

set_window(Screen, 0) -> Screen#screen{current = bottom};
set_window(Screen, 1) -> Screen#screen{current = top}.

set_text_style(#screen{current = top, window_top = WindowTop} = Screen,
	       StyleFlags) ->
    Screen#screen{window_top = WindowTop#text_grid{style = StyleFlags}};
set_text_style(#screen{current = bottom, window_bottom = WindowBottom} = Screen,
	       StyleFlags) ->
    Screen#screen{window_bottom = WindowBottom ++ style_string(StyleFlags)}.

style_string(?STYLE_ROMAN)         -> "{STYLE_ROMAN->}";
style_string(?STYLE_REVERSE_VIDEO) -> "{STYLE_REVERSE_VIDEO->}";
style_string(?STYLE_BOLD)          -> "{STYLE_BOLD->}";
style_string(?STYLE_ITALIC)        -> "{STYLE_ITALIC->}";
style_string(?STYLE_FIXED)         -> "{STYLE_FIXED->}";
style_string(_)                    -> "{STYLE_ROMAN->}".
     
generate_empty_rows(NumRows, NumColumns) ->
    lists:duplicate(NumRows, lists:duplicate(NumColumns, {"{STYLE_ROMAN->}",
							  ?SPACE})).

get_screen3(Screen) ->
    {ObjectName, Value1, Value2} = Screen#screen.status_line,
    WindowBottom = Screen#screen.window_bottom,
    {ok, WindowBottomStr, _} = regexp:gsub(WindowBottom, "\r", "\n"),
    {io_lib:format("~s ~w-~w~n~s",
		   [ObjectName, Value1, Value2, WindowBottomStr]),
    Screen#screen{window_bottom = []}}.

get_screen(Screen) ->
    WindowBottom = Screen#screen.window_bottom,
    {ok, WindowBottomStr, _} = regexp:gsub(WindowBottom, "\r", "\n"),
    {io_lib:format("-------TOP-----------~n~s~n-------BOTTOM--------~n~s",
		   [get_top_window(Screen#screen.window_top), WindowBottomStr]),
    Screen#screen{window_bottom = []}}.

get_top_window(#text_grid{buffer = Rows}) -> get_top_window(Rows);
get_top_window([]) -> "";
get_top_window([Row|Rows]) -> get_row(Row) ++ get_top_window(Rows).

get_row([]) -> "\n";
get_row([{_, Char} | Row]) -> [Char] ++ get_row(Row).
    
