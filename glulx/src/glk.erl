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
%%% Standard Glk API functions are not listed in the export documentation.
%%% Please consult the Glk specification for details on the functionality.
%%% In general a Glk function in Schmalz has the signature
%%% glk_<function name>(VmModule, VmState, GlkArgs)
%%% where VmModule is the Erlang module name for the module containing
%%% VM callbacks, VmState is the state of the virtual machine and GlkArgs
%%% is the parameter list to the Glk function.
%%% This module does not rely on a specific VM implementation (e.g. Glulx),
%%% however, since access to the VM memory is needed, VmModule must export
%%% such memory access methods.
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%%
%%% init()
%%%   initializes the Glk system
%%%
%%% dispatch(VmModule, VmState, Selector, GlkArgs)
%%%   Dispatch to Glk function through a selector
%%%
%%%-----------------------------------------------------------------------

-module(glk).
-export([init/0, dispatch/4, windows/2,
	 glk_put_char/3, glk_put_string/3]).
-include("include/glk.hrl").
-record(glk_state, {vm_module, windows, streams, filerefs, memory_streams,
		    active_stream}).

init() ->
    #glk_state{windows = glk_win:init(), streams = init_streams(),
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

%% Determine the public API function name, useful for printing debug output
%% @spec func_name(int()) -> string().
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

%% Sets a value into an ouput parameter to the Glk function. MemoryRef can
%% either be NULL (do nothing), -1 (push result on stack), or a memory
%% address
%% @spec set_output_param(atom(), vm_state(), int(), int()) -> vmstate().
set_output_param32(_, VmState0, 0, _)                 -> VmState0;
set_output_param32(VmMod, VmState0, -1, Value)        ->
    VmMod:push(VmState0, Value);
set_output_param32(VmMod, VmState0, MemoryRef, Value) ->
    VmMod:set_word32(VmState0, MemoryRef, Value).

%% Sets an array of 32 bit values to the target
%% @spec set_output_param(atom(), vm_state(), int(), [int()]) -> vmstate().
set_output_array32(_, VmState0, 0, _)                            -> VmState0;
set_output_array32(VmMod, VmState0, _, [])                       -> VmState0;
set_output_array32(VmMod, VmState0, -1, [Value|Values])          ->
    set_output_array32(VmMod, VmMod:push(VmState0, Value), -1, Values);
set_output_array32(VmMod, VmState0, MemoryRef, [Value | Values]) ->
    set_output_array32(VmMod, VmMod:set_word32(VmState0, MemoryRef, Value),
		       MemoryRef + 4, Values).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% GLK API functions
%%%%% These are either called directly or through dispatch
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Streams

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

%% Writes a Latin1 character to the currently active stream
glk_put_char(VmMod, VmState0, [Char]) ->
    glk_put_string(VmMod, VmState0, [[Char]]).

%% Writes a string to the currently active stream, this is an optimization
%% function
glk_put_string(VmMod, VmState0, [String]) ->
    io:format("GLK_PUT_STRING ~p~n", [String]),
    GlkState0 = VmMod:glk_state(VmState0),
    CurrentStream = get_stream(GlkState0, GlkState0#glk_state.active_stream),
    GlkState1 = stream_put_string(GlkState0, CurrentStream, String),
    {void, VmMod:set_glk_state(VmState0, GlkState1)}.

%% Sets the output style for the current active stream
glk_set_style(VmMod, VmState0, [Style]) ->
    GlkState0 = VmMod:glk_state(VmState0),
    CurrentStream = get_stream(GlkState0, GlkState0#glk_state.active_stream),
    GlkState1 = stream_set_style(GlkState0, CurrentStream, Style),
    {void, VmMod:set_glk_state(VmState0, GlkState1)}.

glk_stream_open_memory(VmMod, VmState0, [Buf, BufLen, Fmode, Rock]) ->
    GlkState0 = VmMod:glk_state(VmState0),
    {Ref, MemStreams} = memory_add_stream(GlkState0#glk_state.memory_streams,
					  Buf, BufLen, Fmode, Rock),
    GlkState1 = GlkState0#glk_state{memory_streams = MemStreams},
    {StreamId, GlkState2} = add_stream(GlkState1, memory, Ref),
    {StreamId, VmMod:set_glk_state(VmState0, GlkState2)}.

glk_stream_close(VmMod, VmState0, [StreamId, NumCharRef]) ->
    GlkState0 = VmMod:glk_state(VmState0),
    Stream = get_stream(GlkState0, StreamId),
    {VmState1, GlkState1, NumCharsWritten} =
	close_stream_object(VmMod, VmState0, GlkState0, Stream),
    GlkState2 = remove_stream(GlkState1, Stream),
    VmState2 = set_output_array32(VmMod, VmState1, NumCharRef,
				  [0, NumCharsWritten]),
    {void, VmMod:set_glk_state(VmState2, GlkState2)}.

% Windows
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
    {WindowId, GlkWindows} = glk_win:window_open(GlkState0#glk_state.windows,
						 GlkArgs),
    {_StreamId, GlkState1} = add_stream(GlkState0, window, WindowId),
    {WindowId, VmMod:set_glk_state(VmState0,
				   GlkState1#glk_state{windows = GlkWindows})}.

%% Selects the specified window as the current output target
glk_set_window(VmMod, VmState0, [WindowId]) ->
    GlkState0 = VmMod:glk_state(VmState0),
    #glk_state{streams = Streams} = GlkState0,
    GlkState1 = GlkState0#glk_state{active_stream =
				    window_stream(Streams, WindowId)},
    {void, VmMod:set_glk_state(VmState0, GlkState1)}.

% Fileref
glk_fileref_iterate(VmMod, VmState0, [_CurrStream, RockPtr]) ->
    Rock = 0,
    Fileref = 0,
    VmState1 =
	if
	    RockPtr > 0 -> set_output_param32(VmMod, VmState0, RockPtr, Rock);
	    true        -> VmState0
	end,
    {Fileref, VmState1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Streams
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(glk_streams, {nextid, streams}).
-record(glk_stream, {id, type, ref}).

%% initializes the stream system
%% @spec init_streams() -> glk_streams().
init_streams() -> #glk_streams{nextid = 1, streams = []}.

%% Adds a new stream to the stream list
add_stream(#glk_state{streams=#glk_streams{nextid=NextId, streams=Streams}
		      = GlkStreams0} = GlkState0, Type, Ref) ->
    Stream = #glk_stream{id = NextId, type = Type, ref = Ref},
    {NextId, GlkState0#glk_state{
	       streams=GlkStreams0#glk_streams{
			 nextid=NextId + 1, streams=Streams ++ [Stream]}}}.

% dispatch string output to different stream types
stream_put_string(GlkState0, #glk_stream{type = window, ref = WindowId},
		  String) ->
    GlkState0#glk_state{windows=glk_win:put_string(GlkState0#glk_state.windows,
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
    io:format("STREAM_LIST: ~p~n", [StreamList]),
    lists:last(lists:filter(fun (Stream) ->
				    Stream#glk_stream.id =:= StreamId
			    end, StreamList)).

% Only window streams support styles
stream_set_style(GlkState0, #glk_stream{type = window, ref = WindowId},
		 Style) ->
    GlkState0#glk_state{windows = glk_win:set_style(GlkState0#glk_state.windows,
						    WindowId, Style)};
stream_set_style(GlkState0, _, _) -> GlkState0.

%% Closes the object associated with the specified stream
close_stream_object(VmMod, VmState0, GlkState0,
		    #glk_stream{type = memory, ref = Ref}) ->
    {VmState1, MemoryStreams1, NumChars} =
	memory_close_stream(VmMod, VmState0,
			    GlkState0#glk_state.memory_streams, Ref),
    GlkState1 = GlkState0#glk_state{memory_streams = MemoryStreams1},
    {VmState1, GlkState1, NumChars};
close_stream_object(_, _, _, _) -> undef.
    
%% Removes the specified stream from the stream list. If the removed stream
%% was the active stream, set the active stream to null.
remove_stream(#glk_state{streams=#glk_streams{streams=StreamList}} = GlkState0,
	      #glk_stream{id = StreamId} = Stream) ->
    GlkState1 = GlkState0#glk_state{
		  streams = #glk_streams{
		    streams = lists:delete(Stream, StreamList)}},
    % If Stream was current stream, set current stream to null
    io:format("REMOVE_STREAM, ACTIVE STREAM: ~w, StreamId: ~w~n",
	      [GlkState1#glk_state.active_stream, StreamId]),
    case GlkState1#glk_state.active_stream of
	StreamId ->
            io:fwrite("SETTING ACTIVE STREAM TO NULL !!~n"),
	    GlkState1#glk_state{active_stream = null};
	_Default ->
	    GlkState1
    end.

% retrieves a window-typed stream
window_stream([], _) -> undef;
window_stream(#glk_streams{streams = StreamList}, WindowId) ->
    window_stream(StreamList, WindowId);
window_stream([#glk_stream{id = Id, type = window, ref = WindowId} | _Streams],
	       WindowId) -> Id;
window_stream([_ | Streams], WindowId) -> window_stream(Streams, WindowId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Memory Streams
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(memory_streams, {nextid, streams}).
-record(memory_stream, {id, address, length, fmode, rock, buffer}).

init_memory_streams() -> #memory_streams{nextid = 1, streams = []}.

memory_add_stream(#memory_streams{nextid = NextId, streams = Streams}
		  = MemStreams0, Buf, BufLen, Fmode, Rock) ->
    MemStream = #memory_stream{id = NextId, address = Buf, length = BufLen,
			       fmode = Fmode, rock = Rock, buffer = []},
    {NextId, MemStreams0#memory_streams{nextid = NextId + 1,
					streams = Streams ++ [MemStream]}}.

memory_close_stream(VmMod, VmState0, #memory_streams{streams=StreamList}
		    = MemStreams0, Ref) ->
    MemStream = lists:last(lists:filter(fun(#memory_stream{id = Id}) ->
						Id =:= Ref
					end, StreamList)),
    #memory_stream{buffer = Buffer, address = BufAddr, length = BufLen}
	= MemStream,
    VmState1 = memory_write_str8_to_mem(VmMod, VmState0, BufAddr,
					BufAddr + BufLen, Buffer),
    {VmState1,
     MemStreams0#memory_streams{streams=lists:delete(MemStream, StreamList)},
     length(Buffer)}.

%% Writes an 8-Bit String to the specicfied address
memory_write_str8_to_mem(_, VmState0, _, _, [])            -> VmState0;
memory_write_str8_to_mem(_, VmState0, MaxAddr, MaxAddr, _) -> VmState0;
memory_write_str8_to_mem(VmMod, VmState0, Addr, MaxAddr, [Char | String]) ->
    if
	Char > 255 -> Char8 = 16#3f;
	true       -> Char8 = Char
    end,
    memory_write_str8_to_mem(VmMod, VmMod:set_byte(VmState0, Addr, Char8),
			     Addr + 1, MaxAddr, String).
    

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
