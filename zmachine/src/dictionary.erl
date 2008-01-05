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
%%% Description of module dictionary
%%%-----------------------------------------------------------------------
%%% This module handles dictionary access, mhich includes tokenization
%%% and lookup. The standard dictionary is supported for all versions of
%%% the Z-machine.
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%% 
%%% tokenize_and_lookup(Memory, String)
%%%   tokenizes the specicfied ZSCII string and returns a list of
%%%   tuples of the form {position, address, string}
%%%
%%% print(Memory)
%%%   prints all dictionary entries
%%%
%%%-----------------------------------------------------------------------
-module(dictionary).
-vsn('1.0').
-export([tokenize_and_lookup/2, print/1]).
-define(NOT_FOUND, 0).

%% The top-level tokenization function, this function partitions the specified
%% String into a list of token tuples of the form
%% {Position in String, Address in Dictionary, String}, stripping out white
%% space, but leaving in the separators defined in the dictionary
%% @spec tokenize_and_lookup(binary(), string()) -> {int(), int(), string()}
tokenize_and_lookup(Memory, String) ->
    lists:map(fun(Token) -> lookup(Memory, Token) end,
	      tokenize(Memory, String)).

%% Prints the dictionary.
%% @spec print(binary()) -> void()
print(Memory) ->
    {_N, _Separators, _EntryLength, NumEntries} = header(Memory),
    print(Memory, 0, NumEntries - 1).

%%%-----------------------------------------------------------------------
%%% Helper functions
%%%-----------------------------------------------------------------------

tokenize(Memory, String) ->
    lists:append(lists:map(fun(Token) -> tokenize_token(Memory,
							Token) end,
			   tokenize_with_spaces(String))).

tokenize_token(Memory, {GlobalIndex, _String} = Token) ->
    lists:reverse(lists:filter(fun nonempty_token/1,
			       tokenize_with_separators(separators(Memory),
							Token, [], 1,
						        GlobalIndex))).

lookup(Memory, Token) ->
    {_, _, _, NumEntries} = header(Memory),
    MaxWordLength =
	case memory:version(Memory) of
	    3 ->        6;
	    _Default -> 9
	end,
    lookup(Memory, Token, 0, NumEntries - 1, MaxWordLength).

lookup(_Memory, {Pos, String}, Left, Right, _MaxWordLength)
  when Left > Right -> {Pos, ?NOT_FOUND, String};
lookup(Memory, {Pos, String} = Token, Left, Right, MaxWordLength) ->
    Index = (Left + Right) div 2,
    WordAddress = word_address(Memory, Index),
    DictEntry = encoding:decode_address(Memory, WordAddress, WordAddress +
					word_length(memory:version(Memory))),
    %io:format("index: ~p, String: ~p, DictEntry: ~p~n",
	%      [Index, String, DictEntry]),
    case strcmp(substr(String, 1, MaxWordLength), DictEntry) of
	1   -> lookup(Memory, Token, Index + 1, Right, MaxWordLength);
	-1  -> lookup(Memory, Token, Left, Index - 1, MaxWordLength);
	0   -> {Pos, WordAddress, String}
    end.

%% lexicographical comparison of two strings
%% if Str1 == Str2 ->  0
%% if Str1 < Str2  -> -1
%% if Str1 > Str2  ->  1
%% @spec(string(), string()) -> int()
strcmp([], []) -> 0;
strcmp([], _Str2) -> -1;
strcmp(_Str1, []) -> 1;
strcmp([Char1 | Str1], [Char2 | Str2]) ->
    Ord1 = encoding:ord(Char1),
    Ord2 = encoding:ord(Char2),
    if
	Ord1 < Ord2 -> -1;
	Ord1 > Ord2 -> 1;
	true        -> strcmp(Str1, Str2)
    end.

%% tokenize_with_separators() takes a list of token pairs, which were
%% generated in tokenize_with_spaces() and splits them further into
%% a potentially larger list of token pairs
%% @spec tokenize_with_separators(string(), {int(), string()},
%%         [{int(), string}], int(), int()) ->[{int(), string()}]
tokenize_with_separators(_Separators, {_TokenIndex, String} , Acc, Index,
			 GlobalIndex)  when Index > length(String) ->
    [ {GlobalIndex, String} | Acc ];
tokenize_with_separators(Separators, {_TokenIndex, String} = Token , Acc,
			 Index, GlobalIndex) ->
    Char = lists:nth(Index, String),
    IsSeparator = lists:member(Char, Separators),
    if 
	IsSeparator ->
	    Prefix = substr(String, 1, Index - 1),
	    SepIndex = GlobalIndex + length(Prefix),
	    tokenize_with_separators(
	      Separators, {SepIndex + 1, string:substr(String, Index + 1)},
	      [{SepIndex, [Char]} | [ {GlobalIndex, Prefix} | Acc ]],
	      1, GlobalIndex + Index);
        true        ->
	    tokenize_with_separators(Separators, Token, Acc, Index + 1,
				     GlobalIndex)
    end.

%% a Z-machine specific tokenise function that divides a string at ZSCII
%% whitespace and returns a list of tokens, represented by a pair of
%% {Position in string, Token}.
%% @spec tokenize_with_spaces(string()) -> [{int(), string()}]
tokenize_with_spaces(String) ->
    lists:reverse(lists:filter(fun nonempty_token/1,
		  tokenize_with_spaces(String, [], 1, 1))).

tokenize_with_spaces(String, Acc, Index, GlobalIndex) 
  when Index > length(String) -> [ {GlobalIndex, String} | Acc];
tokenize_with_spaces(String, Acc, Index, GlobalIndex) ->
    Char = lists:nth(Index, String),
    IsSpace = encoding:is_space(Char),
    if 
	IsSpace ->
	    Prefix = substr(String, 1, Index - 1),
	    tokenize_with_spaces(string:substr(String, Index + 1),
				 [ {GlobalIndex, Prefix} | Acc ],
                                 1, GlobalIndex + Index);
        true        ->
	    tokenize_with_spaces(String, Acc, Index + 1, GlobalIndex)
    end.

nonempty_token({_Position, String}) -> length(String) > 0.

%% a tolerant substring function which returns an empty string when the
%% end index is smaller than the start index
%% @spec substr(string(), int(), int()) -> string()
substr(_String, Start, Stop) when Stop < Start -> [];
substr(String, Start, Stop) -> string:substr(String, Start, Stop).

header(Memory) ->
    Addr = memory:dictionary_address(Memory),
    <<_:Addr/binary, N:8, Separators:N/binary, EntryLength:8, NumEntries:16,
      _/binary>> = Memory,
    {N, Separators, EntryLength, NumEntries}.

separators(Memory) ->
    {_, Separators, _, _} = header(Memory),
    binary_to_list(Separators).

word_length(Version) when Version =< 3 -> 4;
word_length(_Version) -> 6.

% 0-based index
word_address(Memory, Index) ->
    {N, _Separators, EntryLength, _NumEntries} = header(Memory),
    memory:dictionary_address(Memory) + (1 + N + 3) + EntryLength * Index.

print(Memory, Index, Max) ->
    Addr = word_address(Memory, Index),
    DecList = encoding:decode_address(Memory, Addr, undef),
    io:format("~p word decoded: ~p.~n", [Index, DecList]),
    if
	Index < Max -> print(Memory, Index + 1, Max);
	true -> 1
    end.
