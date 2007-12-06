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
%%% Description of module glulx_util
%%%-----------------------------------------------------------------------
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%%
%%%-----------------------------------------------------------------------

-module(glulx_util).
-export([unsigned_to_signed8/1, signed_to_unsigned8/1,
	 unsigned_to_signed16/1, signed_to_unsigned16/1,
	 unsigned_to_signed32/1, signed_to_unsigned32/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Signed/unsigned conversion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(BYTE_UNSIGNED_MAX, 255).
-define(BYTE_SIGNED_MAX,   127).

-define(WORD16_UNSIGNED_MAX, 65535).
-define(WORD16_SIGNED_MAX,   32767).

-define(WORD32_UNSIGNED_MAX, 4294967296).
-define(WORD32_SIGNED_MAX,   2147483647).

unsigned_to_signed8(ByteValue) when ByteValue > ?BYTE_SIGNED_MAX ->
  -(?BYTE_UNSIGNED_MAX - (ByteValue - 1));
unsigned_to_signed8(ByteValue) -> ByteValue.

signed_to_unsigned8(ByteValue) when ByteValue < 0 ->
	?BYTE_SIGNED_MAX - ByteValue;
signed_to_unsigned8(ByteValue) -> ByteValue.

unsigned_to_signed16(WordValue) when WordValue > ?WORD16_SIGNED_MAX ->
  -(?WORD16_UNSIGNED_MAX - (WordValue - 1));
unsigned_to_signed16(WordValue) -> WordValue.

signed_to_unsigned16(WordValue) when WordValue < 0 ->
	?WORD16_SIGNED_MAX - WordValue;
signed_to_unsigned16(WordValue) -> WordValue.

unsigned_to_signed32(WordValue) when WordValue > ?WORD32_SIGNED_MAX ->
  -(?WORD32_UNSIGNED_MAX - (WordValue - 1));
unsigned_to_signed32(WordValue) -> WordValue.

signed_to_unsigned32(WordValue) when WordValue < 0 ->
	?WORD32_SIGNED_MAX - WordValue;
signed_to_unsigned32(WordValue) -> WordValue.
