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
%%% This module represents the Glk system. Despite its complexity, it
%%% was decided to implement the Glk module as an extension to the
%%% virtual machine, since Glk needs to access the VM's memory, which
%%% creates a cyclic communication dependency which is difficult to
%%% solve if both are implemented as processes.
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%%
%%%-----------------------------------------------------------------------

-module(glk).
-export([init/0, dispatch/4, windows/2,
	 glk_put_char/3, glk_put_string/3]).
-include("include/glk.hrl").
-record(glk_state, {vm_module, windows, streams, filerefs, memory_streams,
		    active_stream}).

init() ->
    #glk_state{windows = init_windows(), streams = init_streams(),
	       filerefs = [],
	       memory_streams = init_memory_streams(),
	       active_stream = null}.

windows(VmMod, VmState) ->
    #glk_state{windows = Windows} = VmMod:glk_state(VmState),
    Windows.
    
dispatch(VmMod, VmState0, Selector, GlkArgs) ->
    io:format("GLK-$~8.16.0B: ~p ~p~n",
	      [Selector, func_name(Selector), GlkArgs]),
    Operation = get_op(Selector),
    case Operation of
	undef    ->
	    io:format("NOT implemented, WIN: ~p~n",
		      [windows(VmMod, VmState0)]);
	_Default ->
	    {Result, VmState1} = Operation(VmMod, VmState0, GlkArgs),
	    io:format("WINDOWS: ~p~n", [windows(VmMod, VmState1)]),
	    {Result, VmState1}
    end.

get_op(Selector) ->
    case Selector of
	?GLK_WINDOW_ITERATE      -> fun glk_window_iterate/3;
	?GLK_WINDOW_OPEN         -> fun glk_window_open/3;
	?GLK_SET_WINDOW          -> fun glk_set_window/3;
	?GLK_STREAM_ITERATE      -> fun glk_stream_iterate/3;
	?GLK_STREAM_OPEN_MEMORY  -> fun glk_stream_open_memory/3;
	?GLK_STREAM_CLOSE        -> fun glk_stream_close/3;
	?GLK_STREAM_SET_CURRENT  -> fun glk_stream_set_current/3;
	?GLK_STREAM_GET_CURRENT  -> fun glk_stream_get_current/3;
	?GLK_FILEREF_ITERATE     -> fun glk_fileref_iterate/3;
	?GLK_PUT_CHAR            -> fun glk_put_char/3;
	?GLK_SET_STYLE           -> fun glk_set_style/3;
	_Default -> undef
    end.

%% Determine the public API function name
func_name(Selector) ->
    case Selector of
	?GLK_WINDOW_ITERATE      -> glk_window_iterate;
	?GLK_WINDOW_OPEN         -> glk_window_open;
	?GLK_SET_WINDOW          -> glk_set_window;
	?GLK_STREAM_ITERATE      -> glk_stream_iterate;
	?GLK_STREAM_OPEN_MEMORY  -> glk_stream_open_memory;
	?GLK_STREAM_CLOSE        -> glk_stream_close;
	?GLK_STREAM_SET_CURRENT  -> glk_stream_set_current;
	?GLK_STREAM_GET_CURRENT  -> glk_stream_get_current;
	?GLK_FILEREF_ITERATE     -> glk_fileref_iterate;
	?GLK_PUT_CHAR            -> glk_put_char;
	?GLK_SET_STYLE           -> glk_set_style;
	_Default -> '(unknown glk selector)'
    end. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Common helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Sets a value into an ouput parameter to the Glk function
%% @spec set_output_param(atom(), vm_state(), int(), int()).
set_output_param32(VmMod, VmState0, -1, Value) ->
    VmMod:push(VmState0, Value);
set_output_param32(VmMod, VmState0, MemoryRef, Value) ->
    VmMod:set_word32(VmState0, MemoryRef, Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% GLK API functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Streams
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(glk_streams, {nextid, streams}).
-record(glk_stream, {id, type, ref}).

%% initializes the stream system
%% @spec init_streams() -> glk_streams().
init_streams() -> #glk_streams{nextid = 1, streams = []}.

%% Iterates through the currently available streams (TODO)
glk_stream_iterate(VmMod, VmState0, [_CurrStream, RockPtr]) ->
    Rock = 0,
    Stream = 0,
    VmState1 =
	if
	    RockPtr > 0 -> set_output_param32(VmMod, VmState0, RockPtr, Rock);
	    true        -> VmState0
	end,
    {Stream, VmState1}.

%% Sets the current stream
%% @spec glk_stream_set_current(atom(), vm_state(), [int()] ->
%%         {void, vm_state()}.
glk_stream_set_current(VmMod, VmState0, [StreamId]) ->
    GlkState0 = VmMod:glk_state(VmState0),
    GlkState1 = GlkState0#glk_state{active_stream = StreamId},
    {void, VmMod:set_glk_state(VmState0, GlkState1)}.

%% Returns the current stream number
%% @spec glk_stream_get_current(atom(), vm_state(), int[]) ->
%%         {int(), vm_state()}.
glk_stream_get_current(VmMod, VmState0, _) ->
    #glk_state{active_stream = ActiveStream} = VmMod:glk_state(VmState0),
    case ActiveStream of
	null     -> {0, VmState0};
	_Default -> {ActiveStream, VmState0}
    end.

%% Adds a new stream to the stream list
add_stream(#glk_state{streams=#glk_streams{nextid=NextId, streams=Streams}
		      = GlkStreams0} = GlkState0, Type, Ref) ->
    Stream = #glk_stream{id = NextId, type = Type, ref = Ref},
    {NextId, GlkState0#glk_state{
	       streams=GlkStreams0#glk_streams{
			 nextid=NextId + 1, streams=Streams ++ [Stream]}}}.

%% Writes a Latin1 character to the currently active stream
glk_put_char(VmMod, VmState0, [Char]) ->
    glk_put_string(VmMod, VmState0, [[Char]]).

%% Writes a string to the currently active stream, this is an optimization
%% function
glk_put_string(VmMod, VmState0, [String]) ->
    GlkState0 = VmMod:glk_state(VmState0),
    CurrentStream = get_stream(GlkState0, GlkState0#glk_state.active_stream),
    GlkState1 = stream_put_string(GlkState0, CurrentStream, String),
    {void, VmMod:set_glk_state(VmState0, GlkState1)}.

% dispatch string output to different stream types
stream_put_string(GlkState0, #glk_stream{type = window, ref = WindowId},
		  String) ->
    GlkState0#glk_state{windows = window_put_string(GlkState0#glk_state.windows,
						    WindowId, String)};
stream_put_string(GlkState0, #glk_stream{type = memory, ref = MemoryId},
		  String) ->
    GlkState0#glk_state{memory_streams = memory_put_string(
					   GlkState0#glk_state.memory_streams,
					   MemoryId, String)}.

%% Returns the specified stream from either glk_state or a list of glk_stream.
get_stream(#glk_state{streams=#glk_streams{streams=StreamList}}, StreamId) ->
    get_stream(StreamList, StreamId);
get_stream(StreamList, StreamId) ->
    lists:last(lists:filter(fun (Stream) ->
				    Stream#glk_stream.id =:= StreamId
			    end, StreamList)).

%% Sets the output style for the current active stream
glk_set_style(VmMod, VmState0, [Style]) ->
    GlkState0 = VmMod:glk_state(VmState0),
    CurrentStream = get_stream(GlkState0, GlkState0#glk_state.active_stream),
    GlkState1 = stream_set_style(GlkState0, CurrentStream, Style),
    {void, VmMod:set_glk_state(VmState0, GlkState1)}.

% Only window streams support styles
stream_set_style(GlkState0, #glk_stream{type = window, ref = WindowId},
		 Style) ->
    GlkState0#glk_state{windows = window_set_style(GlkState0#glk_state.windows,
						   WindowId, Style)};
stream_set_style(GlkState0, _, _) -> GlkState0.

glk_stream_open_memory(VmMod, VmState0, [Buf, BufLen, Fmode, Rock]) ->
    GlkState0 = VmMod:glk_state(VmState0),
    {Ref, MemStreams} = add_memory_stream(GlkState0#glk_state.memory_streams,
					  Buf, BufLen, Fmode, Rock),
    GlkState1 = GlkState0#glk_state{memory_streams = MemStreams},
    {StreamId, GlkState2} = add_stream(GlkState1, memory, Ref),
    {StreamId, VmMod:set_glk_state(VmState0, GlkState2)}.

glk_stream_close(VmMod, VmState0, [StreamId, NumCharRef]) ->
    GlkState0 = VmMod:glk_state(VmState0),
    Stream = get_stream(GlkState0, StreamId),
    io:format("CLOSING: ~p~n", [Stream]),
    VmState1 = VmMod:halt_vm(VmState0, "Closing Stream"),
    GlkState1 = close_stream_object(GlkState0, Stream),
    GlkState2 = remove_stream(GlkState1, Stream),
    {void, VmMod:set_glk_state(VmState1, GlkState2)}.

%% Closes the object associated with the specified stream
close_stream_object(GlkState0, #glk_stream{type = memory, ref = Ref}) ->
    GlkState0#glk_state{memory_streams =
			close_memory_stream(
			  GlkState0#glk_state.memory_streams, Ref)};
close_stream_object(_, _) -> undef.
    
%% Removes the specified stream from the stream list. If the removed stream
%% was the active stream, set the active stream to null.
remove_stream(#glk_state{streams=#glk_streams{streams=StreamList}} = GlkState0,
	      #glk_stream{id = StreamId} = Stream) ->
    GlkState1 = GlkState0#glk_state{streams=lists:delete(Stream, StreamList)},
    % If Stream was current stream, set current stream to null
    case GlkState1#glk_state.active_stream of
	StreamId ->
	    GlkState1#glk_state{active_stream = null};
	_Default ->
	    GlkState1
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Glk Window system
%%%%% A window tree is represented by a binary tree which does not
%%%%% satisfy the binary search tree condition, the keys of the node
%%%%% (the window ids) are assigned continuously. This is to keep window
%%%%% management simple. Window operations are implemented using
%%%%% Depth-First-Search, which is easy to implement.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(glk_windows, {nextid, windowtree}).
-record(glk_pair_window, {id, direction, division_type,
			  child1, size1, child2, size2}).
-record(glk_window, {id, type, rock, buffer}).

init_windows() -> #glk_windows{nextid = 1, windowtree = undef}.

%% Top-level window API functions
%%
glk_window_iterate(VmMod, VmState0, [_CurrStream, RockPtr]) ->
    Rock = 0,
    Window = 0,
    VmState1 =
	if
	    RockPtr > 0 -> set_output_param32(VmMod, VmState0, RockPtr, Rock);
	    true        -> VmState0
	end,
    {Window, VmState1}.

%% Opens a new window through splitting a parent window
glk_window_open(VmMod, VmState0, GlkArgs) ->
    GlkState0 = VmMod:glk_state(VmState0),
    {WindowId, GlkWindows} = window_open(GlkState0#glk_state.windows, GlkArgs),
    {_StreamId, GlkState1} = add_stream(GlkState0, window, WindowId),
    {WindowId, VmMod:set_glk_state(VmState0,
				   GlkState1#glk_state{windows = GlkWindows})}.

%% Selects the specified window as the current output target
glk_set_window(VmMod, VmState0, [WindowId]) ->
    GlkState0 = VmMod:glk_state(VmState0),
    #glk_state{streams = #glk_streams{streams = StreamList}} = GlkState0,
    GlkState1 = GlkState0#glk_state{active_stream =
				    window_stream(StreamList, WindowId)},
    {void, VmMod:set_glk_state(VmState0, GlkState1)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local window functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    
window_stream([], _) -> undef;
window_stream([#glk_stream{id = Id, type = window, ref = WindowId} | _Streams],
	       WindowId) -> Id;
window_stream([_ | Streams], WindowId) -> window_stream(Streams, WindowId).

window_set_style(#glk_windows{nextid = NextId, windowtree = WindowTree}
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
      windowtree = window_put_string(WindowTree, WindowId, StyleString)}.

%% All-purpose window printing
window_put_string(#glk_windows{windowtree = WindowTree}
		  = GlkWindows0, WindowId, String) ->
    GlkWindows0#glk_windows{
      windowtree = window_put_string(WindowTree, WindowId, String)};
window_put_string(#glk_window{id = WindowId, buffer = Buffer} = GlkWindow0,
		  WindowId, String) ->
    GlkWindow0#glk_window{buffer = Buffer ++ String};
window_put_string(#glk_window{id = _}, _, _) -> notfound;
window_put_string(#glk_pair_window{child1 = Child1, child2 = Child2}
		  = PairWindow0, WindowId, String) ->
    WindowTree = window_put_string(Child1, WindowId, String),
    case WindowTree of
	notfound ->
	    PairWindow0#glk_pair_window{
	      child2 = window_put_string(Child2, WindowId, String)};
	_Default ->
	    PairWindow0#glk_pair_window{child1 = WindowTree}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Memory Streams
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(memory_streams, {nextid, streams}).
-record(memory_stream, {id, address, length, fmode, rock, buffer}).

init_memory_streams() -> #memory_streams{nextid = 1, streams = []}.

add_memory_stream(#memory_streams{nextid = NextId, streams = Streams}
		  = MemStreams0, Buf, BufLen, Fmode, Rock) ->
    MemStream = #memory_stream{id = NextId, address = Buf, length = BufLen,
			       fmode = Fmode, rock = Rock, buffer = []},
    {NextId, MemStreams0#memory_streams{nextid = NextId + 1,
					streams = Streams ++ [MemStream]}}.

close_memory_stream(#memory_streams{streams=StreamList} = MemStreams0, Ref) ->
    MemStream = lists:last(lists:filter(fun(#memory_stream{id = Id}) ->
						Id =:= Ref
					end, StreamList)),
    io:format("MEMORY BUFFER: ~p~n", [MemStream#memory_stream.buffer]),
    MemStreams0#memory_streams{streams=lists:delete(MemStream, StreamList)}.
    

memory_put_string(#memory_streams{streams = Streams} = MemStreams0, MemoryId,
		  String) ->
    MemStreams0#memory_streams{streams = memory_put_string(Streams, MemoryId,
							   String)};
memory_put_string([#memory_stream{id = MemoryId, buffer = Buffer}
		   = MemoryStream0 | Streams], MemoryId, String) ->
    [MemoryStream0#memory_stream{buffer = Buffer ++ String} | Streams];
memory_put_string([#memory_stream{id = _} = MemoryStream | Streams],
		  MemoryId, String) ->
    [MemoryStream | memory_put_string(Streams, MemoryId, String)].
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% File Refs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

glk_fileref_iterate(VmMod, VmState0, [_CurrStream, RockPtr]) ->
    Rock = 0,
    Fileref = 0,
    VmState1 =
	if
	    RockPtr > 0 -> set_output_param32(VmMod, VmState0, RockPtr, Rock);
	    true        -> VmState0
	end,
    {Fileref, VmState1}.
