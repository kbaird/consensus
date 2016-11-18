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
-spec compose(atom(), [any()]) -> [any()].
compose(bargaining_proposition, SeatShares) -> compose(bp, SeatShares);
compose(bp, SeatShares) ->
    party_names_min_by(fun length/1, SeatShares);

compose(minimal_connected_winning, SeatShares) -> compose(mcw, SeatShares);
compose(minimum_connected_winning, SeatShares) -> compose(mcw, SeatShares);
compose(mcw, SeatShares) ->
    WithSeats   = winning_coalitions(SeatShares),
    Contiguous  = lists:filter(fun contiguous/1, WithSeats),
    WithinRngs  = lists:filter(fun(C) ->
                      not too_large(C, Contiguous) end,
                  Contiguous),
    just_party_names(WithinRngs);

compose(minimal_range, SeatShares) -> compose(mr, SeatShares);
compose(minimum_range, SeatShares) -> compose(mr, SeatShares);
compose(mr, SeatShares) ->
    party_names_min_by(fun range/1, SeatShares);

compose(minimal_size, SeatShares) -> compose(ms, SeatShares);
compose(minimum_size, SeatShares) -> compose(ms, SeatShares);
compose(ms, SeatShares) ->
    party_names_min_by(fun share/1, SeatShares);

compose(minimal_winning_coalition, SeatShares) -> compose(mwc, SeatShares);
compose(minimum_winning_coalition, SeatShares) -> compose(mwc, SeatShares);
compose(mwc, SeatShares) ->
    WithSeats   = mwc_with_seats(SeatShares),
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
all_in(Inner, All) ->
    lists:all(fun(Elem) -> lists:member(Elem, Inner) end, All).

atom_to_ascii(Atom) -> hd(atom_to_list(Atom)).

centrist_party([Name])    -> Name;
centrist_party([Name, _]) -> Name;
centrist_party(Parties) ->
    Mod = trunc(length(Parties) / 2),
    Sub = lists:sublist(Parties, Mod+1, Mod),
    centrist_party(Sub).

% Does this coalition avoid gaps between parties?
contiguous(Cabinet) ->
    PartyNames  = party_names(Cabinet),
    PartyVals   = [ atom_to_ascii(Name) || Name <- PartyNames ],
    {Lo, Hi}    = party_endpoints(Cabinet),
    AllParties  = lists:seq(atom_to_ascii(Lo), atom_to_ascii(Hi)),
    all_in(PartyVals, AllParties).

is_coalition([ _ ]) -> false;
is_coalition(_)     -> true.

% Does this coalition command a majority of seats?
is_winner(Coalition, SeatShares) ->
    share(Coalition) > share(SeatShares) / 2.0.

just_party_names(Ls) ->
    [ lists:map(fun(Res) -> Res#party_result.name end, L) || L <- Ls ].

party_names_min_by(Fun, SeatShares) ->
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
    [ Res#party_result.name || Res <- Cabinet ].

powerset([]) -> [[]];
powerset([H|T]) ->
    PT = powerset(T),
    powerset(H, PT, PT).

powerset(_, [],    Acc) -> Acc;
powerset(X, [H|T], Acc) -> powerset(X, T, [[X|H]|Acc]).

% How many steps between the "leftmost" partner and the "rightmost" partner?
range(Cabinet) ->
    {Lo, Hi} = party_endpoints(Cabinet),
    atom_to_ascii(Hi) - atom_to_ascii(Lo).

% How many seats does this coalition fill?
share(#party_result{seat_share = Share}) -> Share;
share(L) ->
    lists:foldl(fun(#party_result{seat_share = Share}, Sum) -> Share + Sum end, 0, L).

smallers(Cabinet, Coalitions) ->
    [ Coalition ||  Coalition  <- Coalitions,
                    length(Coalition) < length(Cabinet),
                    all_in(Cabinet, Coalition) ].

% Are any elements in arg2 subsets of arg1?
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
