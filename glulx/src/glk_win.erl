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
%%% Description of module glk_win
%%%-----------------------------------------------------------------------
%%% Glk Window system
%%% A window tree is represented by a binary tree which does not
%%% satisfy the binary search tree condition, the keys of the node
%%% (the window ids) are assigned continuously. This is to keep window
%%% management simple. Window operations are implemented using
%%% Depth-First-Search, which is easy to implement.
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%%
%%% init()
%%%   initializes the Glk window subsystem
%%%
%%% window_open(GlkWindows, GlkArgs)
%%%   opens a new window according to the Glk specification
%%%
%%% put_string(GlkWindows, WindowId, String)
%%%   writes a string to the specified window
%%%
%%% set_style(GlkWindows, WindowId, StyleFlags)
%%%   sets the printing style in the specified window
%%%
%%%-----------------------------------------------------------------------

-module(glk_win).
-export([init/0, window_open/2, put_string/3, set_style/3]).
-include("include/glk.hrl").

-record(glk_windows, {nextid, windowtree}).
-record(glk_pair_window, {id, direction, division_type,
			  child1, size1, child2, size2}).
-record(glk_window, {id, type, rock, buffer}).

init() -> #glk_windows{nextid = 1, windowtree = undef}.

window_open(#glk_windows{nextid = NextId, windowtree = undef}
	    = GlkWindows0, [0, _, _, Wintype, Rock]) ->
    io:format("OPENING INITIAL WINDOW, "
	      "WinType: ~w, Rock: ~w~n", [Wintype, Rock]),
    Window = create_window(NextId, Wintype, Rock),
    GlkWindows1 = GlkWindows0#glk_windows{nextid = NextId + 1,
					  windowtree = Window},
    {Window#glk_window.id, GlkWindows1};
window_open(#glk_windows{nextid = NextId, windowtree = WindowTree}
	    = GlkWindows0, [Split, Method, Size, Wintype, Rock]) ->
    io:format("OPENING WINDOW (TODO), Split: ~w, Method: ~w, Size: ~w, "
	      "WinType: ~w, Rock: ~w~n", [Split, Method, Size, Wintype, Rock]),
    Window = create_window(NextId, Wintype, Rock),
    GlkWindows1 = GlkWindows0#glk_windows{
		    nextid = NextId + 2,
		    windowtree = split_window(WindowTree, Split,
					      Method, Size,
					      Window)},
    {Window#glk_window.id, GlkWindows1}.

split_window(#glk_window{id = Split} = ParentWindow,
	     Split, Method, Size, ChildWindow) ->
    create_pair_window(ParentWindow, ChildWindow, Method, Size);
split_window(#glk_window{id = _SomeId}, _Split, _, _, _) -> notfound;
split_window(#glk_pair_window{child1 = Child1, child2 = Child2},
	     Split, Method, Size, ChildWindow) ->
    WindowTree = split_window(Child1, Split, Method, Size, ChildWindow),
    if
	WindowTree =:= notfound ->
	    split_window(Child2, Split, Method, Size, ChildWindow);
	true ->
	    WindowTree
    end.

create_window(Id, ?WINTYPE_TEXTBUFFER, Rock) ->
    #glk_window{id = Id, type = textbuffer, rock = Rock, buffer = []};
create_window(Id, ?WINTYPE_TEXTGRID, Rock) ->    
    #glk_window{id = Id, type = textgrid, rock = Rock, buffer = []}.

create_pair_window(ParentWindow, #glk_window{id = ChildId} = ChildWindow,
		   Method, Size) ->
    case Method band ?WINMETHOD_DIRMASK of
	?WINMETHOD_LEFT  ->
	    Direction = horizontal,
	    First = ChildWindow,
	    Second = ParentWindow;
	?WINMETHOD_RIGHT ->
	    Direction = horizontal,
	    First = ParentWindow,
	    Second = ChildWindow;
	?WINMETHOD_ABOVE ->
	    Direction = vertical,
	    First = ChildWindow,
	    Second = ParentWindow;
	?WINMETHOD_BELOW ->
	    Direction = vertical,
	    First = ParentWindow,
	    Second = ChildWindow
    end,
    case Method band ?WINMETHOD_DIVISIONMASK of
	?WINMETHOD_FIXED        ->
	    DivisionType = fixed,
	    if
		First =:= ParentWindow ->
		    Size1 = undef,
		    Size2 = Size;
		true                   ->
		    Size1 = Size,
		    Size2 = undef
	    end;
	?WINMETHOD_PROPORTIONAL ->
	    DivisionType = proportional,
	    if
		First =:= ParentWindow ->
		    Size1 = 100 - Size,
		    Size2 = Size;
		true                   ->
		    Size1 = Size,
		    Size2 = 100 - Size
	    end
    end,
    #glk_pair_window{id = ChildId + 1, direction = Direction,
		     division_type = DivisionType,
		     child1 = First, size1 = Size1,
		     child2 = Second, size2 = Size2}.
    
set_style(#glk_windows{nextid = NextId, windowtree = WindowTree}
	  = GlkWindows0, WindowId, Style) ->
    case Style of
	?STYLE_NORMAL       -> StyleString = "{STYLE_NORMAL->}";
	?STYLE_EMPHASIZED   -> StyleString = "{STYLE_EMPHASIZED->}";
	?STYLE_PREFORMATTED -> StyleString = "{STYLE_PREFORMATTED->}";
	?STYLE_HEADER       -> StyleString = "{STYLE_HEADER->}";
	?STYLE_SUBHEADER    -> StyleString = "{STYLE_SUBHEADER->}";
	?STYLE_ALERT        -> StyleString = "{STYLE_ALERT->}";
	?STYLE_NOTE         -> StyleString = "{STYLE_NOTE->}";
	?STYLE_BLOCKQUOTE   -> StyleString = "{STYLE_BLOCKQUOTE->}";
	?STYLE_INPUT        -> StyleString = "{STYLE_INPUT->}";
	?STYLE_USER1        -> StyleString = "{STYLE_USER1->}";
	?STYLE_USER2        -> StyleString = "{STYLE_USER2->}";
	_Default            -> StyleString = "{STYLE_UNDEF->}"
    end,
    GlkWindows0#glk_windows{
      nextid = NextId,
      windowtree = put_string(WindowTree, WindowId, StyleString)}.

%% All-purpose window printing
put_string(#glk_windows{windowtree = WindowTree}
		  = GlkWindows0, WindowId, String) ->
    GlkWindows0#glk_windows{
      windowtree = put_string(WindowTree, WindowId, String)};
put_string(#glk_window{id = WindowId, buffer = Buffer} = GlkWindow0,
		  WindowId, String) ->
    GlkWindow0#glk_window{buffer = Buffer ++ String};
put_string(#glk_window{id = _}, _, _) -> notfound;
put_string(#glk_pair_window{child1 = Child1, child2 = Child2}
		  = PairWindow0, WindowId, String) ->
    WindowTree = put_string(Child1, WindowId, String),
    case WindowTree of
	notfound ->
	    PairWindow0#glk_pair_window{
	      child2 = put_string(Child2, WindowId, String)};
	_Default ->
	    PairWindow0#glk_pair_window{child1 = WindowTree}
    end.
