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
%%% Description of module encoding.
%%%-----------------------------------------------------------------------
%%% This module handles ZSCII strings and decoding.
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%% decode_address(Memory, Address, MaxAddress)
%%%   decodes the encoded ZSCII string at the specified address
%%%   MaxAddress specifies the maximum address for dictionary entries
%%%
%%% num_zencoded_bytes(Memory, Address)
%%%   determine the length of the string at the specified address in bytes
%%%
%%% is_space(Character)
%%%   determine if a given characser is a space character in ZSCII
%%%
%%% ord(Character)
%%%   returns an ordinal number for a specific character, used to correctly
%%%   perform binary searches in the dictionary
%%%
%%%-----------------------------------------------------------------------
-module(encoding).
-vsn('1.0').
-export([decode_address/3, num_zencoded_bytes/2, is_space/1, ord/1]).
-include("include/zscii.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Macros
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(WORD16_LEN, 2).
-define(SHIFT1, 4).
-define(SHIFT2, 5).
-define(word_address(Addr), Addr * 2).
-define(is_shift(Zchar), Zchar =:= 4; Zchar =:= 5).
-define(is_abbrev(Zchar), Zchar =:= 1; Zchar =:= 2; Zchar =:= 3).
-define(is_alphabet(Zchar), Zchar >= 6, Zchar =< 31).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decodes the encoded ZSCII sequence at the specified address
%% if MaxAddress is not "undef", a length check will be performed
%% @spec decode_address(binary(), int(), int()) -> string().
decode_address(Memory, Address, MaxAddress) ->
    decode_list(extract_list(Memory, Address, MaxAddress), Memory).

%% Determine the number of bytes the Z-encoded string at the specified address
%% occupies.
%% @spec num_zencoded_bytes(binary(), int()) -> int().
num_zencoded_bytes(Memory, Address) ->
    <<Bit:1, _First:5, _Second:5, _Third:5>> =
	memory:get_bytes(Memory, Address, 2),
    if
	Bit =:= 1 -> 2;
	Bit =:= 0 -> 2 + num_zencoded_bytes(Memory, Address + 2)
    end.

%% Returns true if Char is a valid space character in ZSCII, false otherwise
%% @spec is_space(int()) -> bool().
is_space(Char) -> Char =:= ?SPACE.

%% The standard dictionary entries are ordered in Z encoding order,
%% this function maps each character to an ordinal number, so a binary
%% search correctly finds entries with special characters
%% @spec ord(int()) -> int().
ord(Char) ->
    case Char of
	?DOLLAR        -> 4;
	?NEWLINE       -> 5;
	16#30          -> 6; % 0-9
	16#31          -> 7;
	16#32          -> 8;
	16#33          -> 9;
	16#34          -> 10;
	16#35          -> 11;
	16#36          -> 12;
	16#37          -> 13;
	16#38          -> 14;
	?DOT           -> 15;
	?COMMA         -> 16;
	?EXCLAIMATION  -> 17;
	?QUESTION_MARK -> 18;
	?UNDERSCORE    -> 19;
	?HASH_MARK     -> 20;
	?SINGLE_QUOTE  -> 21;
	?DOUBLE_QUOTE  -> 22;
	?SLASH         -> 23;
	?BACKSLASH     -> 24;
	?DASH          -> 25;
	?COLON         -> 26;
	?LEFT_PAREN    -> 27;
	?RIGHT_PAREN   -> 28;
	_Default       -> Char
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% retrieves the specified entry from the abbreviations table
%% @spec abbrev(binary(), int(), int()) -> string. 
abbrev(Memory, CurrentChar, NextChar) ->
    EntryAddr = memory:abbrev_address(Memory) +
	((32 * (CurrentChar - 1) + NextChar) * ?WORD16_LEN),
    decode_address(Memory, ?word_address(memory:get_word16(Memory, EntryAddr)),
		   undef).

%% decodes the specified list of 5 bit entries into a list of ZSCII
%% characters
%% @spec()
decode_list(Input, Memory) ->
    {_Alphabet, _NewInput, Result} = decode_it({a0, Input, [], Memory}),
    lists:reverse(Result).

% The decoding function takes a list of Z encoded characters and
% decodes it to a list of valid ZSCII characters.
decode_it({Alphabet, [], Output, _Memory}) -> {Alphabet, [], Output};
% lookup, the alphabet is reset to a0 after that
decode_it({Alphabet, [Zchar|Input], Output, Memory}) ->
    if
	?is_shift(Zchar)  ->
	    decode_it({shift(Alphabet, Zchar), Input, Output, Memory});
	Zchar =:= 0 ->
	    decode_it({a0, Input, [?SPACE | Output], Memory});
	?is_abbrev(Zchar) ->
	    [NextChar | Input2] = Input,
	    Abbrev = lists:reverse(abbrev(Memory, Zchar, NextChar)),
	    decode_it({a0, Input2, Abbrev ++ Output, Memory});
	?is_alphabet(Zchar) ->
	    {Char, NewInput} = lookup(Alphabet, [Zchar|Input]),
	    decode_it(transform(a0, Char, NewInput, Output, Memory));
	true ->
	    % ignoring invalid character
	    decode_it({Alphabet, Input, Output, Memory})
	end.

% alphabet shifting rules
shift(a0, ?SHIFT1) -> a1;
shift(a0, ?SHIFT2) -> a2;
shift(a1, ?SHIFT1) -> a2;
shift(a1, ?SHIFT2) -> a0;
shift(a2, ?SHIFT1) -> a0;
shift(a2, ?SHIFT2) -> a1.

% helper that transforms the tupel given to decode() depending on
% the result of the lookup, undef values are ignored and not appended
% to the Ouput list
transform(Alphabet, undef, NewInput, Output, Memory) ->
    {Alphabet, NewInput, Output, Memory};
transform(Alphabet, Char, NewInput, Output, Memory) ->
    {Alphabet, NewInput, [Char|Output], Memory}.

% lookup characters in an alphabet
lookup(a0, [Zchar|Input]) -> {$a + (Zchar - 6), Input};
lookup(a1, [Zchar|Input]) -> {$A + (Zchar - 6), Input};
lookup(a2, [Zchar|Input]) -> lookup_a2([Zchar|Input]).

% Alphabet A2 does not have its characters in an order corresponding
% to the ASCII character set. The lookup method is therefore defined
% explicitly
lookup_a2([6, Hi, Lo | Input]) -> {Hi bsl 5 bor Lo, Input};
lookup_a2([6, _Hi]) -> {undef, []};
lookup_a2([6]) -> {undef, []};
lookup_a2([7|Input]) -> {?NEWLINE, Input};
lookup_a2([Zchar|Input]) when Zchar >= 8, Zchar < 18 ->
    {$0 + (Zchar - 8), Input};
lookup_a2([Zchar|Input]) ->
    Char = case Zchar of
	       18 -> ?DOT;           % '.'
	       19 -> ?COMMA;         % ','
	       20 -> ?EXCLAIMATION;  % '!'
	       21 -> ?QUESTION_MARK; % '?'
	       22 -> ?UNDERSCORE;    % '_'
	       23 -> ?HASH_MARK;     % '#'
	       24 -> ?SINGLE_QUOTE;  % '\''
	       25 -> ?DOUBLE_QUOTE;  % '"'
	       26 -> ?SLASH;         % '/'
	       27 -> ?BACKSLASH;     % '\'
	       28 -> ?DASH;          % '-'
	       29 -> ?COLON;         % ':'
	       30 -> ?LEFT_PAREN;    % '('
	       31 -> ?RIGHT_PAREN    % ')'
	   end,
    {Char, Input}.

% Extracts the encoded characters at the specified address and
% writes them to a list in reverse order
% if MaxAddress is not "undef", this function will perform a length check
extract_list(List, Memory, Address, MaxAddress) ->
    <<Bit:1, First:5, Second:5, Third:5>> =
	memory:get_bytes(Memory, Address, 2),
    if
	MaxAddress =/= undef, Address >= MaxAddress -> List;
	Bit =:= 1 -> [Third, Second, First | List];
	Bit =:= 0 ->
	    extract_list([Third, Second, First | List], Memory, Address + 2,
			 MaxAddress)
    end.

% this is the toplevel function to extract the encoded word at the specified
% address
extract_list(Memory, Address, MaxAddress) ->
    lists:reverse(extract_list([], Memory, Address, MaxAddress)).
