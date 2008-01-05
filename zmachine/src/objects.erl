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
%%% Description of module objects
%%%-----------------------------------------------------------------------
%%% This module handles object access.
%%%-----------------------------------------------------------------------
%%% Exports
%%%-----------------------------------------------------------------------
%%%
%%% insert_object(Memory, ObjectNum, NewParentNum)
%%%   inserts the specified object as the first child of its new parent
%%%
%%% remove_object(Memory, ObjectNum)
%%%   removes the specified object from the current context 
%%%
%%% has_attribute(Memory, ObjectNum, AttributeNum)
%%%   checks an object for a specific attribute
%%%
%%% set_attribute(Memory, ObjectNum, AttributeNum)
%%%   sets the specified object attribute
%%%
%%% clear_attribute(Memory, ObjectNum, AttributeNum)
%%%   clears the specified object attribute
%%%
%%% parent(Memory, ObjectNum)
%%%   retrieves the parent number for an object
%%%
%%% child(Memory, ObjectNum)
%%%   retrieves the child number for an object
%%%
%%% sibling(Memory, ObjectNum)
%%%   retrieves the sibling number for an object
%%%
%%% name(Memory, ObjectNum)
%%%   retrieves the short name of the specified object
%%%
%%% property_address(Memory, ObjectNum, PropertyNum)
%%%   retrieves the address of an object's property
%%%
%%% property(Memory, ObjectNum, PropertyNum)
%%%   retrieves the specified object property
%%%
%%% next_property_num(Memory, ObjectNum, PropertyNum)
%%%   retrieves the property number following the specified object property
%%%
%%% set_property(Memory, ObjectNum, PropertyNum, Value)
%%%   sets the specified object property
%%%
%%% property_length(Memory, PropertyDataAddress)
%%%   retrieves the length of the property at the specified address
%%%
%%%-----------------------------------------------------------------------
-module(objects).
-vsn('1.0').
-export([insert_object/3, remove_object/2, has_attribute/3, set_attribute/3,
	 clear_attribute/3, parent/2, child/2,
	 sibling/2, property_address/3, property/3, property_length/2,
	 next_property_num/3, set_property/4, name/2]).
-record(object, { attributes, parent, sibling, child, properties }).
-define(MASK_ATTRIBUTES_32, 2#10000000000000000000000000000000).
-define(ALL_BITS_32,        2#11111111111111111111111111111111).
-define(MASK_ATTRIBUTES_48, 2#100000000000000000000000000000000000000000000000).
-define(ALL_BITS_48,        2#111111111111111111111111111111111111111111111111).
-define(MASK_BIT7,          2#10000000).
-define(MASK_BIT6,          2#01000000).
-define(MASK_LOWER_6_BITS,  2#00111111).

%% insert the object with ObjectNum as the first child of the object
%% with the number NewParentNum, the former first child of NewParent
%% will be the first sibling of the inserted object.
%% @spec insert_object(binary(), int(), int()) -> binary()
insert_object(Memory0, ObjectNum, NewParentNum) ->
    Memory1 = remove_object(Memory0, ObjectNum),
    % insert in new parent
    Memory2 = set_parent(Memory1, ObjectNum, NewParentNum),
    Memory3 = set_sibling(Memory2, ObjectNum, child(Memory2, NewParentNum)),
    set_child(Memory3, NewParentNum, ObjectNum).

%% removes the specified object completely out of its previous
%% context
%% @spec remove_object(binary(), int()) -> binary()
remove_object(Memory0, ObjectNum) ->
    PreviousSiblingNum = previous_sibling(Memory0, ObjectNum,
					  parent(Memory0, ObjectNum)),
    Memory1 = case PreviousSiblingNum of
	% first child, attach sibling as the first child
	0        -> set_child(Memory0, parent(Memory0, ObjectNum),
			      sibling(Memory0, ObjectNum));
	_Default -> set_sibling(Memory0, PreviousSiblingNum,
				sibling(Memory0, ObjectNum))
    end,
    set_parent(set_sibling(Memory1, ObjectNum, 0), ObjectNum, 0).
    
%% checks if the an object has a specific attribute and returns true or
%% false
%% @spec has_attribute(binary(), int(), int()) -> bool()
has_attribute(Memory, ObjectNum, AttributeNum) ->
    #object{attributes = Attributes} = get_object(Memory, ObjectNum),
    Version = memory:version(Memory),
    if
	Version =:= 3 ->
	    AttributeMask = ?MASK_ATTRIBUTES_32 bsr AttributeNum;
	true          ->
	    AttributeMask = ?MASK_ATTRIBUTES_48 bsr AttributeNum
    end,
    Attributes band AttributeMask =:= AttributeMask.

%% Sets the specified object attribute.
%% @spec set_attribute(binary(), int(), int()) -> bool()
set_attribute(Memory, ObjectNum, AttributeNum) ->
    Version = memory:version(Memory),
    ObjectAddress = object_address(Memory, ObjectNum, Version),
    if
	Version =:= 3 ->
	    <<Start:ObjectAddress/binary, Attributes:32, End/binary>> = Memory,
	    NewAttributes = Attributes bor
		(?MASK_ATTRIBUTES_32 bsr AttributeNum),
	    <<Start:ObjectAddress/binary, NewAttributes:32, End/binary>>;
	true          ->
	    <<Start:ObjectAddress/binary, Attributes:48, End/binary>> = Memory,
	    NewAttributes = Attributes bor
		(?MASK_ATTRIBUTES_48 bsr AttributeNum),
	    <<Start:ObjectAddress/binary, NewAttributes:48, End/binary>>
    end.

%% Clears the specified object attribute.
%% @spec clear_attribute(binary(), int(), int()) -> bool()
clear_attribute(Memory, ObjectNum, AttributeNum) ->
    Version = memory:version(Memory),
    ObjectAddress = object_address(Memory, ObjectNum, Version),
    if
	Version =:= 3 ->
	    <<Start:ObjectAddress/binary, Attributes:32, End/binary>> = Memory,
	    ClearMask = (?MASK_ATTRIBUTES_32 bsr AttributeNum)
		bxor ?ALL_BITS_32,
	    NewAttributes = Attributes band ClearMask,
	    <<Start:ObjectAddress/binary, NewAttributes:32, End/binary>>;
	true ->
	    <<Start:ObjectAddress/binary, Attributes:48, End/binary>> = Memory,
	    ClearMask = (?MASK_ATTRIBUTES_48 bsr AttributeNum)
		bxor ?ALL_BITS_48,
	    NewAttributes = Attributes band ClearMask,
	    <<Start:ObjectAddress/binary, NewAttributes:48, End/binary>>
    end.

%% retrieves the parent of the specified object
%% @spec parent(binary(), int()) -> int()
parent(Memory, ObjectNum) ->
    #object{parent = Parent} = get_object(Memory, ObjectNum),
    Parent.

%% retrieves the child of the specified object
%% @spec child(binary(), int()) -> int()
child(Memory, ObjectNum) ->
    #object{child = Child} = get_object(Memory, ObjectNum),
    Child.

%% retrieves the sibling of the specified object
%% @spec sibling(binary(), int()) -> int()
sibling(Memory, ObjectNum) ->
    #object{sibling = Sibling} = get_object(Memory, ObjectNum),
    Sibling.

%% Retrieves the short name of the specified object as a list of
%% ZSCII characters
%% @spec name(binary(), int()) -> [int()]
name(Memory, ObjectNum) ->
    #object{properties = PropTableAddress} = get_object(Memory, ObjectNum),
    encoding:decode_address(Memory, PropTableAddress + 1, undef).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Property functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% retrieves the address of an object's property data.
%% @spec property_address(binary(), int(), int()) -> int()
property_address(Memory, ObjectNum, PropertyNum) ->
    object_prop_addr(Memory, PropertyNum,
		     object_prop_addr0(Memory, ObjectNum)).

%% Retrieves the property number following the specified property number.
%% If PropertyNum is 0, return the first property number of the object
%% @spec next_property_num(binary(), int(), int()) -> int()
next_property_num(Memory, ObjectNum, 0) ->
    property_num(Memory, object_prop_addr0(Memory, ObjectNum));
next_property_num(Memory, ObjectNum, PropertyNum) ->
    PropDataAddr = property_address(Memory, ObjectNum, PropertyNum),
    property_num(Memory, PropDataAddr + property_length(Memory, PropDataAddr)).

%% retrieves the specified object property
%% @spec property(binary(), int(), int()) -> int()
property(Memory, ObjectNum, PropertyNum) ->
    PropertyDataAddress = property_address(Memory, ObjectNum, PropertyNum),
    property_or_default(Memory, PropertyNum, PropertyDataAddress).

%% sets the specified object property
%% @spec set_property(binary(), int(), int(), int()) -> binary()
set_property(Memory, ObjectNum, PropertyNum, Value) ->
    PropertyDataAddress = property_address(Memory, ObjectNum, PropertyNum),
    PropertySize =
	property_length(Memory, PropertyDataAddress),
    case PropertySize of
	1 ->
	    memory:set_byte(Memory, PropertyDataAddress, Value band 16#ff);
	2 ->
	    memory:set_word16(Memory, PropertyDataAddress, Value band 16#ffff);
	_Default ->
	    io:format("@set_property invalid property size: ~p~n",
		      [PropertySize]),
	    undef
    end.

%% retrieves the length of the property which data resides at the
%% specified address.
%% @spec property_length(binary(), int()) -> int()
property_length(Memory, PropertyDataAddress) ->
    {_, PropertyLength} = property_info(Memory, PropertyDataAddress),
    PropertyLength.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Property Table Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(property_length_v3(PropertySizeByte), PropertySizeByte div 32 + 1).

%% address of first property, same for all versions
%% @spec object_prop_addr0(binary(), int()) -> int()
object_prop_addr0(Memory, ObjectNum) ->
    #object{properties = PropTableAddress} = get_object(Memory, ObjectNum),
    HeaderSize = memory:get_byte(Memory, PropTableAddress) * 2 + 1,
    PropTableAddress + HeaderSize.

%% Given a property data address, extract the property number and
%% length
property_info(Memory, PropertyDataAddress) ->
    case memory:version(Memory) of
	3        -> property_info_v3(Memory, PropertyDataAddress);
	_Default -> property_info_std(Memory, PropertyDataAddress)
    end.

inform_property_length(0)              -> 64;
inform_property_length(PropertyLength) -> PropertyLength.

num_propsize_bytes(Memory, PropertyAddress) ->
    case memory:version(Memory) of
	3        -> 1;
	_Default ->
	    SizeByte1 = memory:get_byte(Memory, PropertyAddress),
	    if 
		SizeByte1 band ?MASK_BIT7 =:= ?MASK_BIT7 -> 2;
		true -> 1
	    end
    end.
    
object_prop_addr(Memory, PropertyNum, PropertyAddress) ->
    {CurrentProp, PropertyLength} =
	property_info(Memory, PropertyAddress +
		      num_propsize_bytes(Memory, PropertyAddress)),
    if
	CurrentProp =:= 0 -> 0;
	true              -> object_prop_addr(Memory, PropertyNum,
					      PropertyAddress,
					      CurrentProp, PropertyLength)
    end.

object_prop_addr(Memory, PropertyNum, PropertyAddress,
		       CurrentProp, PropertyLength) ->
    if
	PropertyNum > CurrentProp   -> 0; % no need to search further
	CurrentProp =:= PropertyNum ->
	    PropertyAddress + num_propsize_bytes(Memory, PropertyAddress);
	true                        ->
	    object_prop_addr(Memory, PropertyNum, PropertyAddress +
			     num_propsize_bytes(Memory, PropertyAddress) +
			     PropertyLength)
    end.

property_num(Memory, PropertyAddress) ->
    {PropertyNum, _PropertyLength} =
	property_info(Memory, PropertyAddress +
		      num_propsize_bytes(Memory, PropertyAddress)),
    PropertyNum.

property_or_default(Memory, PropertyNum, 0)                    ->
    property_default(Memory, PropertyNum);
property_or_default(Memory, _PropertyNum, PropertyDataAddress) ->
    PropertySize =
	property_length(Memory, PropertyDataAddress),
    case PropertySize of
	1        -> memory:get_byte(Memory, PropertyDataAddress);
	2        -> memory:get_word16(Memory, PropertyDataAddress);
        _Default -> undef 
    end.

property_default(Memory, PropertyNum) ->
    memory:get_word16(Memory, memory:object_table_address(Memory) +
		      (PropertyNum - 1) * 2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% V4-8-specific property helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

property_info_std(Memory, PropertyDataAddress) ->
    BytePrev = memory:get_byte(Memory, PropertyDataAddress - 1),
    HasTwo = (BytePrev band ?MASK_BIT7) =:= ?MASK_BIT7,
    if
	HasTwo ->
	    SizeByte1 = memory:get_byte(Memory,
					PropertyDataAddress - 2),
	    PropertyNum = SizeByte1 band ?MASK_LOWER_6_BITS,
	    PropertyLength = BytePrev band ?MASK_LOWER_6_BITS;
	true   ->
	    PropertyNum = BytePrev band ?MASK_LOWER_6_BITS,
	    PropertyLength = case (BytePrev band ?MASK_BIT6) of
		?MASK_BIT6 -> 2;
		_Default  -> 1
	    end	    
    end,
    {PropertyNum, inform_property_length(PropertyLength)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% V3-specific property helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

property_info_v3(Memory, PropertyDataAddress) ->
    PropertySizeByte = memory:get_byte(Memory, PropertyDataAddress - 1),
    PropertyLength = ?property_length_v3(PropertySizeByte),
    PropertyNum = PropertySizeByte - 32 * (PropertyLength - 1),
    {PropertyNum, PropertyLength}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Object Access Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% retrieves the previous sibling number of ObjectNum in the child list of
% ParentNum, if ObjectNum is the first child, 0 will be returned
previous_sibling(Memory, ObjectNum, ParentNum) ->
    #object{child = FirstChildNum} = get_object(Memory, ParentNum),
    if
	ParentNum     =:= 0;
	FirstChildNum =:= ObjectNum -> 0;
	true                        -> object_with_sibling(Memory,
							   FirstChildNum,
							   ObjectNum)
    end.

object_with_sibling(Memory, ObjectNum, NextChildNum) ->
    #object{sibling = SiblingNum} = get_object(Memory, ObjectNum),
    if
	SiblingNum =:= NextChildNum -> ObjectNum;
	true                        -> object_with_sibling(Memory, SiblingNum,
							   NextChildNum)
    end.

% - set the new parent's first child as the next sibling of the new child
% Set parent for versions 1-3
set_parent(Memory, ObjectNum, NewParentNum) ->  
    Version = memory:version(Memory),
    ObjectAddress = object_address(Memory, ObjectNum, Version),
    if
	Version =:= 3 ->
	    <<Start:ObjectAddress/binary, _Attributes:32, _Parent:8,
	     _Sibling:8, _Child:8, _Properties:16, End/binary>> = Memory,
	    <<Start:ObjectAddress/binary, _Attributes:32, NewParentNum:8,
	     _Sibling:8, _Child:8, _Properties:16, End/binary>>;
	true ->
	    <<Start:ObjectAddress/binary, _Attributes:48, _Parent:16,
	     _Sibling:16, _Child:16, _Properties:16, End/binary>> = Memory,
	    <<Start:ObjectAddress/binary, _Attributes:48, NewParentNum:16,
	     _Sibling:16, _Child:16, _Properties:16, End/binary>>
    end.

set_child(Memory, ObjectNum, NewChildNum) ->  
    Version = memory:version(Memory),
    ObjectAddress = object_address(Memory, ObjectNum, Version),
    if
	Version =:= 3 ->
	    <<Start:ObjectAddress/binary, _Attributes:32, _Parent:8,
	     _Sibling:8, _Child:8, _Properties:16, End/binary>> = Memory,
	    <<Start:ObjectAddress/binary, _Attributes:32, _Parent:8,
	     _Sibling:8, NewChildNum:8, _Properties:16, End/binary>>;
	true ->
	    <<Start:ObjectAddress/binary, _Attributes:48, _Parent:16,
	     _Sibling:16, _Child:16, _Properties:16, End/binary>> = Memory,
	    <<Start:ObjectAddress/binary, _Attributes:48, _Parent:16,
	     _Sibling:16, NewChildNum:16, _Properties:16, End/binary>>
    end.

set_sibling(Memory, ObjectNum, NewSiblingNum) ->  
    Version = memory:version(Memory),
    ObjectAddress = object_address(Memory, ObjectNum, Version),
    if
	Version =:= 3 ->
	    <<Start:ObjectAddress/binary, _Attributes:32, _Parent:8,
	     _Sibling:8, _Child:8, _Properties:16, End/binary>> = Memory,
	    <<Start:ObjectAddress/binary, _Attributes:32, _Parent:8,
	     NewSiblingNum:8, _Child:8, _Properties:16, End/binary>>;
	true ->
	    <<Start:ObjectAddress/binary, _Attributes:48, _Parent:16,
	     _Sibling:16, _Child:16, _Properties:16, End/binary>> = Memory,
	    <<Start:ObjectAddress/binary, _Attributes:48, _Parent:16,
	     NewSiblingNum:16, _Child:16, _Properties:16, End/binary>>
    end.

% Extract the components of an object entry which was retrieved by
% get_object_entry. This is more a demo function
% This is for story version 1-3
get_object(Memory, Object) ->
    Version = memory:version(Memory),
    ObjectAddress = object_address(Memory, Object, Version),
    if
	Version =:= 3 ->
	    <<_:ObjectAddress/binary, Attributes:32, Parent:8, Sibling:8,
	      Child:8, Properties:16, _/binary>> = Memory;
	true          ->
	    <<_:ObjectAddress/binary, Attributes:48, Parent:16, Sibling:16,
	      Child:16, Properties:16, _/binary>> = Memory
    end,
    #object{attributes = Attributes, parent = Parent, sibling = Sibling,
	    child = Child, properties = Properties}.

%% retrieves the address of an object
%% @spec object_address(binary(), int(), int()) -> int()
object_address(Memory, Object, Version) ->
    memory:object_table_address(Memory) + num_bytes_prop_defaults(Version) +
	(Object - 1) * obj_table_entry_size(Version).

% helpers to calculate the proper offsets
num_bytes_prop_defaults(Version) when Version =< 3 -> 31 * 2;
num_bytes_prop_defaults(_Version)                  -> 63 * 2.

% size of an object table entry
obj_table_entry_size(Version) when Version =< 3 -> 9;
obj_table_entry_size(_Version) -> 14.
