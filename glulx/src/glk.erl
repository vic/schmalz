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
%%% Description of module glk
%%%-----------------------------------------------------------------------
%%% This module represents the GLK system. Since it is a system which
%%% is more complex than the Glulx VM, it is modelled as an autonomous
%%% Erlang process, effectively encapsulating its state.
%%% In order to set values in the VM's memory, every API call returns
%%% a list of virtual callbacks that might be performed before processing
%%% the result. The virtual callback solution avoids that there is a
%%% circular dependency between the VM and GLK, which would be difficult
%%% to implement in Erlang.
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%%
%%%-----------------------------------------------------------------------

-module(glk).
-export([init/0, rpc/2]).
-include("include/glk.hrl").
-record(glk_state, {windows, streams, filerefs, active_stream}).

init() ->
    GlkState = #glk_state{windows = init_windows(), streams = init_streams(),
			  filerefs = [], active_stream = null},
    spawn(fun() -> listen(GlkState) end).

rpc(GlkPid, Message) ->
    GlkPid ! {self(), Message},
    receive
	{GlkPid, Response} -> Response
    after 500 ->
        io:format("waiting for ack timed out"),
	halt
    end.

listen(GlkState0) ->
    receive
	{From, {call, Selector, GlkArgs}} ->
	    io:format("GLK-$~8.16.0B: ~p ~p~n",
		      [Selector, func_name(Selector), GlkArgs]),
	    Operation = get_op(Selector),
	    {GlkResult, GlkState1} = Operation(GlkState0, GlkArgs),
	    %io:format("GLK API RESULT ~p~n", [Result]),
	    io:format("GLK STATE ~p~n", [GlkState1]),
	    ack(From, GlkResult),
	    listen(GlkState1);
	{From, Other} ->
	    ack(From, {error, Other}),
	    listen(GlkState0)
    end.

ack(Pid, Message) -> Pid ! {self(), Message}.

get_op(Selector) ->
    case Selector of
	?GLK_WINDOW_ITERATE  -> fun glk_window_iterate/2;
	?GLK_WINDOW_OPEN     -> fun glk_window_open/2;
	?GLK_SET_WINDOW      -> fun glk_set_window/2;
	?GLK_STREAM_ITERATE  -> fun glk_stream_iterate/2;
	?GLK_FILEREF_ITERATE -> fun glk_fileref_iterate/2;
	?GLK_PUT_CHAR        -> fun glk_put_char/2;
	_Default -> undef
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% GLK API functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Streams
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(glk_stream, {id, type, ref}).
init_streams() -> {1, []}.

glk_stream_iterate(GlkState0, [_CurrStream, RockPtr]) ->
    Rock = 0,
    Stream = 0,
    if
	RockPtr > 0 -> VmCallbacks = [{set_word32, [RockPtr, Rock]}];
	true        -> VmCallbacks = []
    end,
    {?GLK_RESULT_CB(Stream, VmCallbacks), GlkState0}.

add_stream(#glk_state{streams = {NextId, Streams}} = GlkState0, Type, Ref) ->
    Stream = #glk_stream{id = NextId, type = Type, ref = Ref},
    GlkState0#glk_state{streams = {NextId + 1, Streams ++ [Stream]}}.

glk_put_char(#glk_state{active_stream = ActiveStreamId,
			streams = {_, Streams}} = GlkState0, [Char]) ->
    CurrentStream = get_stream(Streams, ActiveStreamId),
    GlkState1 = stream_put_char(GlkState0, CurrentStream, Char),
    {?GLK_RESULT_VOID, GlkState1}.

stream_put_char(GlkState0, #glk_stream{type = window, ref = WindowId}, Char) ->
    window_put_char(GlkState0, WindowId, Char).

get_stream(Streams, ActiveStreamId) ->
    lists:last(lists:filter(fun (Stream) ->
				    Stream#glk_stream.id =:= ActiveStreamId
			    end, Streams)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Glk Window system
%%%%% A window tree is represented by a binary tree which does not
%%%%% satisfy the binary search tree condition, the keys of the node
%%%%% (the window ids) are assigned continuously. This is to keep window
%%%%% management simple. Window operations are implemented using
%%%%% Depth-First-Search, which is easy to implement.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(WINTYPE_ALL,            0).
-define(WINTYPE_PAIR,           1).
-define(WINTYPE_BLANK,          2).
-define(WINTYPE_TEXTBUFFER,     3).
-define(WINTYPE_TEXTGRID,       4).
-define(WINTYPE_GRAPHICS,       5).

-define(WINMETHOD_LEFT,         16#0).
-define(WINMETHOD_RIGHT,        16#1).
-define(WINMETHOD_ABOVE,        16#2).
-define(WINMETHOD_BELOW,        16#3).
-define(WINMETHOD_DIRMASK,      16#f).
-define(WINMETHOD_FIXED,        16#10).
-define(WINMETHOD_PROPORTIONAL, 16#20).
-define(WINMETHOD_DIVISIONMASK, 16#f0).

-record(glk_pair_window, {id, direction, division_type,
			  child1, size1, child2, size2}).
-record(glk_window, {id, type, rock, buffer}).

init_windows() -> {1, undef}.

glk_window_iterate(GlkState0, [_CurrWindow, RockPtr]) ->
    Rock = 0,
    Window = 0,
    if
	RockPtr > 0 -> VmCallbacks = [{set_word32, [RockPtr, Rock]}];
	true        -> VmCallbacks = []
    end,
    {?GLK_RESULT_CB(Window, VmCallbacks), GlkState0}.

glk_window_open(#glk_state{windows = {NextId, undef}} = GlkState0,
	    [0, _, _, Wintype, Rock]) ->
    io:format("OPENING INITIAL WINDOW, "
	      "WinType: ~w, Rock: ~w~n", [Wintype, Rock]),
    Window = create_window(NextId, Wintype, Rock),
    GlkState1 = GlkState0#glk_state{windows = {NextId + 1, Window}},
    {?GLK_RESULT(Window#glk_window.id),
     add_stream(GlkState1, window, NextId)};
glk_window_open(#glk_state{windows = {NextId, WindowTree}} = GlkState0,
	    [Split, Method, Size, Wintype, Rock]) ->
    io:format("OPENING WINDOW (TODO), Split: ~w, Method: ~w, Size: ~w, "
	      "WinType: ~w, Rock: ~w~n", [Split, Method, Size, Wintype, Rock]),
    Window = create_window(NextId, Wintype, Rock),
    GlkState1 = GlkState0#glk_state{
		  windows = {NextId + 2, split_window(WindowTree, Split, Method,
						      Size, Window)}},
    {?GLK_RESULT(Window#glk_window.id),
     add_stream(GlkState1, window, NextId)}.

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
    

glk_set_window(#glk_state{streams = {_, Streams}} = GlkState0, [WindowId]) ->
    {?GLK_RESULT_VOID,
     GlkState0#glk_state{active_stream = window_stream(Streams, WindowId)}}.

window_stream([], _) -> undef;
window_stream([#glk_stream{id = Id, type = window, ref = WindowId} | _Streams],
	       WindowId) -> Id;
window_stream([_ | Streams], WindowId) -> window_stream(Streams, WindowId).

window_put_char(#glk_state{windows = {_, WindowTree}} = GlkState0,
		WindowId, Char) ->
    GlkState0#glk_state{windows = window_put_char2(WindowTree, WindowId, Char)}.

window_put_char2(#glk_window{id = WindowId, buffer = Buffer} = GlkWindow0,
		 WindowId, Char) ->
    GlkWindow0#glk_window{buffer = Buffer ++ [Char]};
window_put_char2(#glk_window{id = _SomeWindowId}, _WindowId, _) -> notfound;
window_put_char2(#glk_pair_window{child1 = Child1, child2 = Child2},
		 WindowId, Char) ->
    WindowTree = window_put_char2(Child1, WindowId, Char),
    case WindowTree of
	notfound -> window_put_char2(Child2, WindowId, Char);
	_Default -> WindowTree
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% File Refs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

glk_fileref_iterate(GlkState0, [_CurrFileref, RockPtr]) ->
    Rock = 0,
    Fileref = 0,
    if
	RockPtr > 0 -> VmCallbacks = [{set_word32, [RockPtr, Rock]}];
	true        -> VmCallbacks = []
    end,
    {?GLK_RESULT_CB(Fileref, VmCallbacks), GlkState0}.

%% Determine the public API function name
func_name(Selector) ->
    case Selector of
	?GLK_WINDOW_ITERATE  -> glk_window_iterate;
	?GLK_WINDOW_OPEN     -> glk_window_open;
	?GLK_SET_WINDOW      -> glk_set_window;
	?GLK_STREAM_ITERATE  -> glk_stream_iterate;
	?GLK_FILEREF_ITERATE -> glk_fileref_iterate;
	?GLK_PUT_CHAR        -> glk_put_char;
	_Default -> '(unknown glk selector)'
    end. 
