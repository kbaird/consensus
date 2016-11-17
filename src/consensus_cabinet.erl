-module(consensus_cabinet).

%% API exports
-export([
    compose/2
]).

-include("parties.hrl").

%%====================================================================
%% API functions
%%====================================================================

% Cf. Lijphart, Arend, _Patterns of Democracy_, 1999. pg93.
-spec compose(atom(), [{party_name(), number()}]) -> [any()].
compose(bargaining_proposition, SeatShares) -> compose(bp, SeatShares);
compose(bp, SeatShares) ->
    min_by(fun length/1, SeatShares);


compose(minimal_connected_winning, SeatShares) -> compose(mcw, SeatShares);
compose(minimum_connected_winning, SeatShares) -> compose(mcw, SeatShares);
compose(mcw, SeatShares) ->
    WithSeats  = winning_coalitions(SeatShares),
    Contiguous = lists:filter(fun contiguous/1, WithSeats),
    WithinRngs = lists:filter(fun(C) ->
                    not too_large(C, Contiguous) end,
                 Contiguous),
    just_party_names(WithinRngs);


compose(minimal_range, SeatShares) -> compose(mr, SeatShares);
compose(minimum_range, SeatShares) -> compose(mr, SeatShares);
compose(mr, SeatShares) ->
    min_by(fun range/1, SeatShares);


compose(minimal_size, SeatShares) -> compose(ms, SeatShares);
compose(minimum_size, SeatShares) -> compose(ms, SeatShares);
compose(ms, SeatShares) ->
    min_by(fun share/1, SeatShares);


compose(minimal_winning_coalition, SeatShares) -> compose(mwc, SeatShares);
compose(minimum_winning_coalition, SeatShares) -> compose(mwc, SeatShares);
compose(mwc, SeatShares) ->
    WithSeats  = mwc_with_seats(SeatShares),
    just_party_names(WithSeats);

compose(policy_viable_coalition, SeatShares) -> compose(pvc, SeatShares);
compose(pvc, SeatShares) ->
    Cabinets    = mwc_with_seats(SeatShares),
    PartyNames  = party_names(SeatShares),
    CenterPty   = centrist_party(PartyNames),
    WithCtrPty  = [ Cab ||  Cab <- Cabinets,
                            lists:member(CenterPty, party_names(Cab)) ],
    just_party_names(WithCtrPty).

%%====================================================================
%% Internal functions
%%====================================================================
atom_to_ascii(Atom) -> hd(atom_to_list(Atom)).

centrist_party([Name])    -> Name;
centrist_party([Name, _]) -> Name;
centrist_party(Parties) ->
    Len = length(Parties),
    Mod = trunc(Len / 2),
    Sub = lists:sublist(Parties, Mod+1, Mod),
    centrist_party(Sub).

contiguous(Cabinet) ->
    PartyNames  = party_names(Cabinet),
    PartyVals   = [ atom_to_ascii(Name) || Name <- PartyNames ],
    {Lo, Hi}    = party_endpoints(Cabinet),
    AllParties  = lists:seq(atom_to_ascii(Lo), atom_to_ascii(Hi)),
    lists:all(fun(PartyVal) ->
                lists:member(PartyVal, PartyVals)
              end, AllParties).

is_coalition([{_, _}]) -> false;
is_coalition(_)       -> true.

is_winner(Coalition, SeatShares) ->
    share(Coalition) > share(SeatShares) / 2.0.

just_party_names(Ls) ->
    [ lists:map(fun({Name, _Cnt}) -> Name end, L) || L <- Ls ].

min_by(Fun, SeatShares) ->
    Cabs    = mwc_with_seats(SeatShares),
    Vals    = [ Fun(C) || C <- Cabs ],
    MinVal  = lists:min(Vals),
    WithSeats = [ Cab || Cab <- Cabs, Fun(Cab) =:= MinVal ],
    just_party_names(WithSeats).

mwc_with_seats(SeatShares) ->
    Winners = winning_coalitions(SeatShares),
    lists:filter(fun(C) -> not too_large(C, Winners) end, Winners).

party_endpoints(Cabinet) ->
    PartyNames  = party_names(Cabinet),
    SortedNames = lists:sort(PartyNames),
    [ Hi | _ ]  = lists:reverse(SortedNames),
    [ Lo | _ ]  = SortedNames,
    {Lo, Hi}.

party_names(Cabinet) ->
    [ PartyName || {PartyName, _} <- Cabinet ].

powerset([]) -> [[]];
powerset([H|T]) ->
    PT = powerset(T),
    powerset(H, PT, PT).

powerset(_, [],    Acc) -> Acc;
powerset(X, [H|T], Acc) ->
    powerset(X, T, [[X|H]|Acc]).

range(Cabinet) ->
    {Lo, Hi} = party_endpoints(Cabinet),
    atom_to_ascii(Hi) - atom_to_ascii(Lo).

share({_Name, Cnt}) -> Cnt;
share(L) ->
    lists:foldl(fun({_N, Cnt}, Sum) -> Cnt + Sum end, 0, L).

smallers(Cabinet, Coalitions) ->
    [ Coalition ||  Coalition  <- Coalitions,
                    length(Coalition) < length(Cabinet),
                    lists:all(fun(Party) ->
                        lists:member(Party, Cabinet)
                    end, Coalition) ].

too_large(C, Coalitions) ->
    Smallers = smallers(C, Coalitions),
    length(Smallers) > 0.

uniqueify(Ls) ->
    lists:usort([ lists:usort(L) || L <- Ls ]).

winning_coalitions(SeatShares) ->
    PSet        = powerset(SeatShares),
    Unique      = uniqueify(PSet),
    Coalitions  = lists:filter(fun is_coalition/1, Unique),
    lists:filter(fun(C) -> is_winner(C, SeatShares) end, Coalitions).
