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
	 print_zscii/2, split_window/2]).

-record(screen, {status_line, window_top, window_bottom, current}).

create(NumColumns) ->
    #screen{status_line = {"(Empty)", 0, 0},
	    window_top = {0, NumColumns, []},
	    window_bottom = [], current = bottom}.

status_line(#screen{status_line = {ObjectName, Value1, Value2}}) ->
    {ObjectName, Value1, Value2}.

set_status_line(Screen, ObjectName, Value1, Value2) ->
    Screen#screen{status_line = {ObjectName, Value1, Value2}}.

bottom_window(#screen{window_bottom = WindowBottom} = Screen) ->
  {WindowBottom, Screen#screen{window_bottom = []}}.

print_zscii(#screen{window_bottom = Window} = Screen, ZsciiString) ->
    Screen#screen{window_bottom = Window ++ ZsciiString}.

split_window(#screen{window_top = {_NumLines, NumColumns, Rows}}
		    = Screen, NewNumRows) ->
    Diff = NewNumRows - length(Rows),
    NewRows =
	if
	    Diff =< 0 -> Rows;
	    true -> Rows ++ generate_empty_rows(Diff, NumColumns)
    end,
    Screen#screen{window_top = {NewNumRows, NumColumns, NewRows}}.

generate_empty_rows(NumRows, NumColumns) ->
    lists:duplicate(NumRows, lists:duplicate(NumColumns, {[], " "})).
