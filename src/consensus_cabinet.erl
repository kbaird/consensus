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
-spec compose(atom(), [party_result()]) -> [cabinet()].
compose(bargaining_proposition, SeatShares) -> compose(bp, SeatShares);
compose(bp, SeatShares) ->
    party_names_min_by(fun length/1, SeatShares);

compose(minimal_connected_winning, SeatShares) -> compose(mcw, SeatShares);
compose(minimum_connected_winning, SeatShares) -> compose(mcw, SeatShares);
compose(mcw, SeatShares) ->
    WithSeats   = winning_coalitions(SeatShares),
    Contiguous  = lists:filter(fun is_contiguous/1, WithSeats),
    SmallEnough = fun(C) -> not too_large(C, Contiguous) end,
    InRange     = lists:filter(SmallEnough, Contiguous),
    just_party_names(InRange);

compose(minimal_range, SeatShares) -> compose(mr, SeatShares);
compose(minimum_range, SeatShares) -> compose(mr, SeatShares);
compose(mr, SeatShares) ->
    party_names_min_by(fun range/1, SeatShares);

compose(minimal_size, SeatShares) -> compose(ms, SeatShares);
compose(minimum_size, SeatShares) -> compose(ms, SeatShares);
compose(ms, SeatShares) ->
    party_names_min_by(fun seat_share/1, SeatShares);

compose(minimal_winning_coalition, SeatShares) -> compose(mwc, SeatShares);
compose(minimum_winning_coalition, SeatShares) -> compose(mwc, SeatShares);
compose(mwc, SeatShares) ->
    WithSeats   = mwc_with_seats(SeatShares),
    just_party_names(WithSeats);

compose(policy_viable_coalition, SeatShares) -> compose(pvc, SeatShares);
compose(pvc, SeatShares) ->
    Cabinets    = mwc_with_seats(SeatShares),
    CenterPty   = centrist_party(party_names(SeatShares)),
    WithCtrPty  = [ Cab ||  Cab <- Cabinets,
                            lists:member(CenterPty, party_names(Cab)) ],
    just_party_names(WithCtrPty).

%%====================================================================
%% Internal functions
%%====================================================================
all_in(Inner, All) ->
    lists:all(fun(Elem) -> lists:member(Elem, Inner) end, All).

atom_to_ascii(Atom) -> hd(atom_to_list(Atom)).

-spec centrist_party([party_name()]) -> party_name().
centrist_party([Name])    -> Name;
centrist_party([Name, _]) -> Name;
centrist_party(Parties) ->
    Mod = trunc(length(Parties) / 2),
    Sub = lists:sublist(Parties, Mod+1, Mod),
    centrist_party(Sub).

% Does this coalition avoid gaps between parties?
-spec is_contiguous(cabinet()) -> boolean().
is_contiguous(Cabinet) ->
    PartyNames  = party_names(Cabinet),
    PartyVals   = lists:map(fun atom_to_ascii/1, PartyNames),
    {Lo, Hi}    = party_endpoints(Cabinet),
    % This will not work with real party names,
    % but works for the current single letter codes
    AllParties  = lists:seq(atom_to_ascii(Lo), atom_to_ascii(Hi)),
    all_in(PartyVals, AllParties).

-spec is_coalition(cabinet()) -> boolean().
is_coalition(Cab) -> length(Cab) > 1.

% Does this coalition command a majority of seats?
-spec is_winner([cabinet()], [cabinet()]) -> boolean().
is_winner(Coalition, SeatShares) ->
    seat_share(Coalition) > seat_share(SeatShares) / 2.0.

-spec just_party_names([cabinet()]) -> [[atom()]].
just_party_names(Cabs) ->
    [ party_names(Cab) || Cab <- Cabs ].

party_names_min_by(Fun, SeatShares) ->
    Cabs      = mwc_with_seats(SeatShares),
    Vals      = lists:map(Fun, Cabs),
    WithSeats = [ Cab || Cab <- Cabs, Fun(Cab) =:= lists:min(Vals) ],
    just_party_names(WithSeats).

mwc_with_seats(SeatShares) ->
    Winners = winning_coalitions(SeatShares),
    lists:filter(fun(C) -> not too_large(C, Winners) end, Winners).

-spec party_endpoints(cabinet()) -> {party_name(), party_name()}.
party_endpoints(Cabinet) ->
    PartyNames  = party_names(Cabinet),
    SortedNames = lists:sort(PartyNames),
    [ Hi | _ ]  = lists:reverse(SortedNames),
    [ Lo | _ ]  = SortedNames,
    {Lo, Hi}.

-spec party_names(cabinet()) -> [party_name()].
party_names(Cabinet) ->
    [ consensus_party:name(P) || P <- Cabinet ].

powerset([])    -> [[]];
powerset([H|T]) ->
    PT = powerset(T),
    powerset(H, PT, PT).

powerset(_, [],    Acc) -> Acc;
powerset(X, [H|T], Acc) -> powerset(X, T, [[X|H]|Acc]).

% How many steps between the "leftmost" partner and the "rightmost" partner?
-spec range(cabinet()) -> number().
range(Cabinet) ->
    {Lo, Hi} = party_endpoints(Cabinet),
    atom_to_ascii(Hi) - atom_to_ascii(Lo).

% How many seats does this coalition fill?
-spec seat_share([party_result()]) -> number().
seat_share(PartyResults) when is_list(PartyResults) ->
    SumShares = fun(P, Acc) -> consensus_party:seat_share(P) + Acc end,
    lists:foldl(SumShares, 0, PartyResults);
seat_share(PartyResult) -> seat_share([PartyResult]).

% Are any elements in arg2 subsets of arg1?
-spec too_large(cabinet(), [cabinet()]) -> boolean().
too_large(Cabinet, Coalitions) ->
    Smallers = [ Coalition || Coalition  <- Coalitions,
                              length(Coalition) < length(Cabinet),
                              all_in(Cabinet, Coalition) ],
    length(Smallers) > 0.

uniqueify(Ls) ->
    lists:usort([ lists:usort(L) || L <- Ls ]).

-spec winning_coalitions([cabinet()]) -> [cabinet()].
winning_coalitions(SeatShares) ->
    PSet        = powerset(SeatShares),
    Unique      = uniqueify(PSet),
    Coalitions  = lists:filter(fun is_coalition/1, Unique),
    lists:filter(fun(C) -> is_winner(C, SeatShares) end, Coalitions).
