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
%%% Description of module glk.hrl
%%%-----------------------------------------------------------------------
%%%
%%%-----------------------------------------------------------------------

-define(GLK_WINDOW_ITERATE,  16#20).
-define(GLK_WINDOW_OPEN,     16#23).
-define(GLK_SET_WINDOW,      16#2f).
-define(GLK_STREAM_ITERATE,  16#40).
-define(GLK_FILEREF_ITERATE, 16#64).
-define(GLK_PUT_CHAR,        16#80).
-define(GLK_SET_STYLE,       16#86).

-define(GLK_RESULT(RetVal), {glk_result, RetVal, []}).
-define(GLK_RESULT_CB(RetVal, VmCallbacks), {glk_result, RetVal, VmCallbacks}).
-define(GLK_RESULT_VOID, {glk_result, 0, []}).

-define(call_glk(Message), glk:rpc(GlkPid, Message)).
