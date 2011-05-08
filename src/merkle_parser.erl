%%%-------------------------------------------------------------------
%%% Copyright 2011, Andrew Oswald
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @author andrew oswald
%%% @doc 
%%%      Performs "allkeys" and "diff" operations on byte[] (bitstring)
%%%      merkle trees marshaled from a JMerkle process.
%%% @end
%%%-------------------------------------------------------------------
-module(merkle_parser).

-compile(native).

-export([allkeys/1, diff/2]).

-define(LeafByte, 0).
-define(BranchByte, 1).
-define(BranchIdBytes, 27).

-define(key_conversion(X,BinaryKey), <<X:1/big-signed-integer-unit:8>> = BinaryKey).

-define(TopBranch, << ?BranchByte, HashVal:20/bytes, ChildrenOffset:32, ChildCount:16, Children/binary >>).
-define(InnerBranch, << Key, ?BranchByte, HashVal:20/bytes, ChildrenOffset:32 ChildCount:16, Children/binary >>).

-define(SingleLeaf, << ?LeafByte, HashVal:20/bytes, UserKeyOffset:32, UserKey:UserKeyOffset/bytes >>).
-define(Leaf, << Key, ?LeafByte, HashVal:20/bytes, UserKeyOffset:32, UserKey:UserKeyOffset/bytes >>).
-define(InnerLeaf, << Key, ?LeafByte, HashVal:20/bytes, UserKeyOffset:32, UserKey:UserKeyOffset/bytes, OtherTrees/binary >>).

%% ==================================================================
%% API Function Definitions
%% ==================================================================

%% -------------------------------------------------------------------
%% @doc Given a merkle tree bitstring, provides all of its keys as
%% a list of bitstrings.
%% -------------------------------------------------------------------

allkeys(<<>>) ->
	% empty binary; return an empty list.
	[];

allkeys(<< ?BranchByte, _BranchNoise:26/bytes, Children/binary >>) ->
	% top branch; return allkeys/2 of its Children
	allkeys(Children, []);

allkeys(<< ?LeafByte, _HashVal:20/bytes, UserKeyOffset:32, UserKey:UserKeyOffset/bytes >>) ->
	% single leaf; return the UserKey bytes.
	[UserKey].

%% -------------------------------------------------------------------
%% @doc Given two merkle tree bitstrings, provides their differing
%% keys as a list of bitstrings.
%% -------------------------------------------------------------------

diff(<<>>, X) when is_binary(X) ->
	allkeys(X);

diff(X, <<>>) when is_binary(X) ->
	allkeys(X);

diff(<< ?BranchByte, HashVal:20/bytes, _Branch1Noise/binary >>,
	 << ?BranchByte, HashVal:20/bytes, _Branch2Noise/binary >>) ->
	[];

diff(<< ?LeafByte, HashVal:20/bytes, UserKeyOffset:32, UserKey:UserKeyOffset/bytes >>,
	 << ?LeafByte, HashVal:20/bytes, UserKeyOffset:32, UserKey:UserKeyOffset/bytes >>) ->
	[];

% two leaves w/ same userKey values, but different hashVals; returns leaves' UserKey.
diff(<< ?LeafByte, _Leaf1HashVal:20/bytes, UserKeyOffset:32, UserKey:UserKeyOffset/bytes >>,
	 << ?LeafByte, _Leaf2HashVal:20/bytes, UserKeyOffset:32, UserKey:UserKeyOffset/bytes >>) ->
	[UserKey];

% two leaves w/ differing userKey values; returns UserKey from both leaves.
diff(<< ?LeafByte, _Leaf1HashVal:20/bytes, Leaf1UserKeyOffset:32, Leaf1UserKey:Leaf1UserKeyOffset/bytes >>,
	 << ?LeafByte, _Leaf2HashVal:20/bytes, Leaf2UserKeyOffset:32, Leaf2UserKey:Leaf2UserKeyOffset/bytes >>) ->
	[Leaf1UserKey,Leaf2UserKey];

diff(<< ?BranchByte, _BranchNoise/binary >> = Branch,
	 << ?LeafByte, _LeafNoise/binary >> = Leaf) ->
	diff(Leaf, Branch);

diff(<< ?LeafByte, HashVal:20/bytes, UserKeyOffset:32, UserKey:UserKeyOffset/bytes >>,
	 << ?BranchByte, _BranchHashVal:20/bytes, _ChildrenOffset:32, _ChildCount:16, Children/binary >>) ->
	BranchUserKeys = allkeys(Children,[]),
	case contains({HashVal, UserKey}, Children) of
		false ->
			[UserKey|BranchUserKeys];
		true ->
			lists:delete(UserKey, BranchUserKeys);
		yes_but_hashes_are_different ->
			BranchUserKeys
	end;

diff(<< ?BranchByte, _Branch1Noise:26/bytes, Branch1Children/binary >>,
	 << ?BranchByte, _Branch2Noise:26/bytes, Branch2Children/binary >>) ->
	diff(Branch1Children, Branch2Children, []).


%% ==================================================================
%% Internal Function Definitions
%% ==================================================================

allkeys(<< _Key, ?BranchByte, _HashVal:20/bytes, _ChildrenOffset:32, _ChildCount:16, Children/binary >>, Acc) ->
	allkeys(Children, Acc);

allkeys(<< _Key, ?LeafByte, _HashVal:20/bytes, UserKeyOffset:32, UserKey:UserKeyOffset/bytes >>, Acc) ->
	[UserKey|Acc];

allkeys(<< _Key, ?LeafByte, _HashVal:20/bytes, UserKeyOffset:32, UserKey:UserKeyOffset/bytes, OtherTrees/binary >>, Acc) ->
	allkeys(OtherTrees, [UserKey|Acc]).

contains({HashVal, UserKey}, << _Key, ?LeafByte, ChildHashVal:20/bytes, UserKeyOffset:32, UserKey:UserKeyOffset/bytes, _OtherTrees/binary >>) ->
	case HashVal == ChildHashVal of
		false ->
			yes_but_hashes_are_different;
		true ->
			true
	end;

contains(LeafInfo, << _Key, ?LeafByte, _ChildHashVal:20/bytes, UserKeyOffset:32, _UserKey:UserKeyOffset/bytes, OtherTrees/binary >>) ->
	contains(LeafInfo, OtherTrees);

contains(LeafInfo, << _Key, ?BranchByte, _BranchNoise:26/bytes, Children/binary >>) ->
	contains(LeafInfo, Children);

contains(_,_) ->
	false.


% two empty values.
diff(<<>>,<<>>,Acc) ->
	Acc;

% an empty value and something.
diff(<<>>,X,Acc) ->
	allkeys(X, Acc);

diff(X,<<>>,Acc) ->
	allkeys(X, Acc);

% two equal branches.
diff(<< Key, ?BranchByte, HashVal:20/bytes, ChildrenOffset:32, _B1Noise/binary >> = B1,
	 << Key, ?BranchByte, HashVal:20/bytes, ChildrenOffset:32, _B2Noise/binary >> = B2,
	 Acc) ->
	AdjustedChildrenOffset = ChildrenOffset - ?BranchIdBytes,
	<< Key, ?BranchByte, HashVal:20/bytes, ChildrenOffset:32, _B1ChildCount:16, _B1Children:AdjustedChildrenOffset/bytes, B1Remaining/binary >> = B1,
	<< Key, ?BranchByte, HashVal:20/bytes, ChildrenOffset:32, _B2ChildCount:16, _B2Children:AdjustedChildrenOffset/bytes, B2Remaining/binary >> = B2,
	diff(B1Remaining, B2Remaining, Acc);

% two branches on the same key, but w/ different hashVals.
diff(<< Key, ?BranchByte, _B1HashVal:20/bytes, B1ChildrenOffset:32, _B1Noise/binary >> = B1,
	 << Key, ?BranchByte, _B2HashVal:20/bytes, B2ChildrenOffset:32, _B2Noise/binary >> = B2,
	 Acc) ->
	AdjustedB1ChildrenOffset = B1ChildrenOffset - ?BranchIdBytes,
	AdjustedB2ChildrenOffset = B2ChildrenOffset - ?BranchIdBytes,
	<< Key, ?BranchByte, _B1HashVal:20/bytes, B1ChildrenOffset:32, _B1ChildCount:16, B1Children:AdjustedB1ChildrenOffset/bytes, B1Remaining/binary >> = B1,
	<< Key, ?BranchByte, _B2HashVal:20/bytes, B2ChildrenOffset:32, _B2ChildCount:16, B2Children:AdjustedB2ChildrenOffset/bytes, B2Remaining/binary >> = B2,
	NewAcc = diff(B1Children, B2Children, Acc),
	diff(B1Remaining, B2Remaining, NewAcc);

% two equal leaves.
diff(<< Key, ?LeafByte, HashVal:20/bytes, UserKeyOffset:32, UserKey:UserKeyOffset/bytes, L1Remaining/binary >>,
	 << Key, ?LeafByte, HashVal:20/bytes, UserKeyOffset:32, UserKey:UserKeyOffset/bytes, L2Remaining/binary >>,
	 Acc) ->
	diff(L1Remaining, L2Remaining, Acc);

% two leaves w/ equal keys and equal userKeys, but different hashVals.
diff(<< Key, ?LeafByte, _L1HashVal:20/bytes, UserKeyOffset:32, UserKey:UserKeyOffset/bytes, L1Remaining/binary >>,
	 << Key, ?LeafByte, _L2HashVal:20/bytes, UserKeyOffset:32, UserKey:UserKeyOffset/bytes, L2Remaining/binary >>,
	 Acc) ->
	diff(L1Remaining, L2Remaining, [UserKey|Acc]);

% two leaves w/ equal keys, but different hashVals and different userKeys.
diff(<< Key, ?LeafByte, _L1HashVal:20/bytes, UserKeyOffset:32, L1UserKey:UserKeyOffset/bytes, L1Remaining/binary >>,
	 << Key, ?LeafByte, _L2HashVal:20/bytes, UserKeyOffset:32, L2UserKey:UserKeyOffset/bytes, L2Remaining/binary >>,
	 Acc) ->
	diff(L1Remaining, L2Remaining, [L1UserKey|[L2UserKey|Acc]]);


diff(<< Key, ?BranchByte, _BranchNoise/binary >> = Branch,
	 << Key, ?LeafByte, _LeafNoise/binary >> = Leaf,
	 Acc) ->
	diff(Leaf, Branch, Acc);

diff(<< Key, ?LeafByte, HashVal:20/bytes, UserKeyOffset:32, UserKey:UserKeyOffset/bytes, LeafRemaining/binary >>,
	 << Key, ?BranchByte, _BranchHashVal:20/bytes, ChildrenOffset:32, _BranchNoise/binary >> = Branch,
	 Acc) ->
	AdjustedChildrenOffset = ChildrenOffset - ?BranchIdBytes,
	<< Key, ?BranchByte, _BranchHashVal:20/bytes, ChildrenOffset:32, _ChildCount:16, Children:AdjustedChildrenOffset/bytes, BranchRemaining/binary >> = Branch,
	% all the leaves in the branch (as per its children offset) and maybe the leaf.
	BranchKeys1 = allkeys(Children, []),
	BranchKeys2 = case contains({HashVal, UserKey}, Children) of
		false ->
			[UserKey|BranchKeys1];
		true ->
			lists:delete(UserKey, BranchKeys1);
		yes_but_hashes_are_different ->
			BranchKeys1
	end,
	% recurse on the remaining bytes.
	diff(LeafRemaining, BranchRemaining, BranchKeys2 ++ Acc);

% different keys
diff(<< Tree1Key, _Tree1Noise/binary >> = Tree1,
	 << Tree2Key, _Tree2Noise/binary >> = Tree2, 
     Acc) ->
	?key_conversion(Num1, <<Tree1Key>>),
	?key_conversion(Num2, <<Tree2Key>>),
	case Num1 < Num2 of
		false ->
			diff1(Tree2,Tree1,Acc);
		true ->
			diff1(Tree1,Tree2,Acc)
	end.

diff1(<< _Key, ?BranchByte, _HashVal:20/bytes, ChildrenOffset:32, _BranchNoise/binary >> = Tree1, Tree2, Acc) ->
	AdjustedChildrenOffset = ChildrenOffset - ?BranchIdBytes,
	<< _Key, ?BranchByte, _HashVal:20/bytes, ChildrenOffset:32, _ChildCount:16, Children:AdjustedChildrenOffset/bytes, Tree1Remaining/binary >> = Tree1,
	NewAcc = allkeys(Children, Acc),
	diff(Tree1Remaining, Tree2, NewAcc);

diff1(<< _Key, ?LeafByte, _HashVal:20/bytes, UserKeyOffset:32, UserKey:UserKeyOffset/bytes, Tree1Remaining/binary >>, Tree2, Acc) ->
	NewAcc = [UserKey|Acc],
	diff(Tree1Remaining, Tree2, NewAcc).