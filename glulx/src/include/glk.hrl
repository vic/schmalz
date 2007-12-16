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

% GLK Selectors
-define(GLK_WINDOW_ITERATE,      16#20).
-define(GLK_WINDOW_OPEN,         16#23).
-define(GLK_SET_WINDOW,          16#2f).
-define(GLK_STREAM_ITERATE,      16#40).
-define(GLK_STREAM_OPEN_MEMORY,  16#43).
-define(GLK_STREAM_SET_CURRENT,  16#47).
-define(GLK_STREAM_GET_CURRENT,  16#48).
-define(GLK_FILEREF_ITERATE,     16#64).
-define(GLK_PUT_CHAR,            16#80).
-define(GLK_SET_STYLE,           16#86).

% Window constants
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

-define(STYLE_NORMAL,           0).
-define(STYLE_EMPHASIZED,       1).
-define(STYLE_PREFORMATTED,     2).
-define(STYLE_HEADER,           3).
-define(STYLE_SUBHEADER,        4).
-define(STYLE_ALERT,            5).
-define(STYLE_NOTE,             6).
-define(STYLE_BLOCKQUOTE,       7).
-define(STYLE_INPUT,            8).
-define(STYLE_USER1,            9).
-define(STYLE_USER2,            10).
