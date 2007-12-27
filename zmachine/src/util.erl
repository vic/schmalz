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
%%% Description of module util
%%%-----------------------------------------------------------------------
%%% Commonly used functionality
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%% read_file(Filename)
%%%   reads the specified file
%%%-----------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Signed/unsigned conversion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(util).
-export([unsigned_to_signed16/1, signed_to_unsigned16/1,
	 unsigned_to_signed14/1, list_replace/3]).

-define(WORD14_UNSIGNED_MAX, 16383).
-define(WORD14_SIGNED_MAX, 8191).

-define(WORD16_UNSIGNED_MAX, 65535).
-define(WORD16_SIGNED_MAX, 32767).

unsigned_to_signed14(WordValue) when WordValue > ?WORD14_SIGNED_MAX ->
  -(?WORD14_UNSIGNED_MAX - (WordValue - 1));
unsigned_to_signed14(WordValue) -> WordValue.

unsigned_to_signed16(WordValue) when WordValue > ?WORD16_SIGNED_MAX ->
  -(?WORD16_UNSIGNED_MAX - (WordValue - 1));
unsigned_to_signed16(WordValue) -> WordValue.

signed_to_unsigned16(WordValue) when WordValue < 0 ->
	?WORD16_SIGNED_MAX - WordValue;
signed_to_unsigned16(WordValue) -> WordValue.

% Zero-based,
list_replace(List, Index, ReplaceElem) ->
    lists:append([lists:sublist(List, Index - 1), [ReplaceElem],
		  lists:sublist(List, Index + 1, length(List) - 1)]).
