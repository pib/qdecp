%%    Copyright 2009 Igor Ribeiro Sucupira.
%%
%%    This module is free software; you can redistribute it and/or
%%    modify it under the terms of the GNU Lesser General Public
%%    License as published by the Free Software Foundation; either
%%    version 2.1 of the License, or (at your option) any later version.
%%
%%    This module is distributed in the hope that it will be useful,
%%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%    Lesser General Public License for more details.


%%%----------------------------------------------------------------------
%%% Implements consistent hashing functionality for fragmented tables
%%%----------------------------------------------------------------------

-module(mnesia_frag_chash).

%%-behaviour(mnesia_frag_hash).

-export([
	 init_state/2,
	 add_frag/1,
	 del_frag/1,
	 key_to_frag_number/2,
	 match_spec_to_frag_numbers/2
	]).

-record(hash_state,
	{n_fragments,
         n_frag_pieces,
         chash_table}).


-define(FRAG_CHASH_LIMIT, 1000000007).
-define(N_FRAG_PIECES, 100).
-define(FRAG_NUM_BITS, 20).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%     Exported functions     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init_state(_Tab, State) when State == undefined ->
    #hash_state{n_fragments   = 1,
                n_frag_pieces = ?N_FRAG_PIECES,
                chash_table   = insert_frag_entries(1, ?N_FRAG_PIECES, ok_gb_sets:new())}.


add_frag(#hash_state{n_fragments = NFragments, n_frag_pieces = NFragPieces, chash_table = ChashTable} = State) ->
    %% TODO: some lookups in ChashTable could find the really affected fragments.
    AffectedFragments = lists:seq(1, NFragments),
    NewState = State#hash_state{n_fragments = NFragments + 1,
                                chash_table = insert_frag_entries(NFragments + 1, NFragPieces, ChashTable)},
    {NewState, AffectedFragments, [NFragments + 1]}.


del_frag(#hash_state{n_fragments = NFragments, n_frag_pieces = NFragPieces, chash_table = ChashTable} = State) ->
    %% TODO: some lookups in ChashTable could find the really affected fragments.
    AffectedFragments = lists:seq(1, NFragments - 1),
    NewState = State#hash_state{n_fragments = NFragments - 1,
                                chash_table = del_frag_entries(NFragments, NFragPieces, ChashTable)},
    {NewState, [NFragments], AffectedFragments}.

key_to_frag_number(#hash_state{chash_table = ChashTable}, Key) ->
    HashVal = erlang:phash2(Key, ?FRAG_CHASH_LIMIT),
    GeqIt = ok_gb_sets:geq_iterator(ChashTable, make_entry(HashVal, 0, 0)),
    case ok_gb_sets:next(GeqIt) of
        none ->
            get_frag_num(ok_gb_sets:smallest(ChashTable));
        {Entry, _} ->
            get_frag_num(Entry)
    end.

match_spec_to_frag_numbers(#hash_state{n_fragments = N} = State, MatchSpec) ->
    case MatchSpec of
        [{HeadPat, _, _}] when is_tuple(HeadPat), tuple_size(HeadPat) > 2 ->
            KeyPat = element(2, HeadPat),
            case mnesia:has_var(KeyPat) of
                false ->
                    %% If the key is known, so is the fragment.
                    [key_to_frag_number(State, KeyPat)];
                true ->
                    lists:seq(1, N)
            end;
        _ ->
            lists:seq(1, N)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%     Internal functions     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_frag_entries(_, NumPieces, ChashTable) when is_integer(NumPieces), NumPieces =< 0 ->
    ChashTable;
insert_frag_entries(FragNum, NumPieces, ChashTable) when is_integer(NumPieces) ->
    HashVal = erlang:phash2({FragNum, NumPieces}, ?FRAG_CHASH_LIMIT),
    NewTable = ok_gb_sets:insert(make_entry(HashVal, FragNum, NumPieces), ChashTable),
    insert_frag_entries(FragNum, NumPieces - 1, NewTable).

del_frag_entries(_, NumPieces, ChashTable) when is_integer(NumPieces), NumPieces =< 0 ->
    ChashTable;
del_frag_entries(FragNum, NumPieces, ChashTable) when is_integer(NumPieces) ->
    HashVal = erlang:phash2({FragNum, NumPieces}, ?FRAG_CHASH_LIMIT),
    NewTable = ok_gb_sets:delete(make_entry(HashVal, FragNum, NumPieces), ChashTable),
    del_frag_entries(FragNum, NumPieces - 1, NewTable).

get_frag_num({_HashVal, FragEntry}) ->
    FragEntry rem (1 bsl ?FRAG_NUM_BITS).

make_entry(HashVal, FragNumber, FragPiece) ->
    {HashVal, (FragPiece bsl ?FRAG_NUM_BITS) bor FragNumber}.
