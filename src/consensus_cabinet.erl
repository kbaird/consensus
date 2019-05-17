-module(consensus_cabinet).

%% API exports
-export([
    compose/2,
    compose/3
]).

-include("include/common.hrl").
-include("include/parties.hrl").

%%====================================================================
%% Guard Macros
%%====================================================================

-define(IS_BP(Label), Label =:= bp orelse Label =:= bargaining_proposition).
-define(IS_MCW(Label), Label =:= mcw orelse
                       Label =:= minimal_connected_winning orelse
                       Label =:= minimum_connected_winning).
-define(IS_MR(Label), Label =:= mr orelse Label =:= minimal_range orelse Label =:= minimum_range).
-define(IS_MS(Label), Label =:= ms orelse Label =:= minimal_size orelse Label =:= minimum_size).
-define(IS_MWC(Label), Label =:= mwc orelse
                       Label =:= minimal_winning_coalition orelse
                       Label =:= minimum_winning_coalition).
-define(IS_PVC(Label), Label =:= pvc orelse Label =:= policy_viable_coalition).

%%====================================================================
%% API functions
%%====================================================================

% Cf. Lijphart, Arend, _Patterns of Democracy_, 1999. pg93.
-spec compose(label(), [party_result()]) -> [cabinet()].
compose(Label, SeatShares) when ?IS_BP(Label) ->
    party_names_min_by(fun length/1, SeatShares);

compose(Label, SeatShares) when ?IS_MR(Label) ->
    party_names_min_by(fun range/1, SeatShares);

compose(Label, SeatShares) when ?IS_MS(Label) ->
    party_names_min_by(fun seat_share/1, SeatShares);

compose(Label, SeatShares) when ?IS_MWC(Label) ->
    WithSeats   = mwc_with_seats(SeatShares),
    just_party_names(WithSeats);

compose(Label, SeatShares) when ?IS_PVC(Label) ->
    Cabinets    = mwc_with_seats(SeatShares),
    CenterPty   = centrist_party(party_names(SeatShares)),
    WithCtrPty  = [ Cab ||  Cab <- Cabinets,
                            lists:member(CenterPty, party_names(Cab)) ],
    just_party_names(WithCtrPty).

% This case requires knowledge of all parties
-spec compose(label(), [party_result()], [party_name()]) -> [cabinet()].
compose(Label, SeatShares, AllParties) when ?IS_MCW(Label) ->
    WithSeats   = winning_coalitions(SeatShares),
    Contiguous  = lists:filter(fun(Coalition) -> is_contiguous(Coalition, AllParties) end, WithSeats),
    SmallEnough = fun(C) -> not is_too_large(C, Contiguous) end,
    InRange     = lists:filter(SmallEnough, Contiguous),
    just_party_names(InRange).

%%====================================================================
%% Internal functions
%%====================================================================
-spec all_in([party_name()], [party_name()]) -> boolean().
all_in(ContainsEachItem, ItemsToCheck) ->
    Present = fun(Elem) -> lists:member(Elem, ContainsEachItem) end,
    lists:all(Present, ItemsToCheck).

-spec centrist_party([party_name()]) -> party_name().
centrist_party([Name])    -> Name;
centrist_party([Name, _]) -> Name;
centrist_party(Parties) ->
    Mod = trunc(length(Parties) / 2),
    Sub = lists:sublist(Parties, Mod+1, Mod),
    centrist_party(Sub).

% Does this coalition avoid gaps between parties?
-spec is_contiguous(cabinet(), [party_name()]) -> boolean().
is_contiguous(Cabinet, AllParties) ->
    PartyNames = party_names(Cabinet),
    [ FirstParty | Tail ] = lists:sort(PartyNames),
    LastParty  = lists:last(Tail),
    Opposition = [AP || AP <- AllParties, not lists:member(AP, PartyNames)],
    Forbidden  = [OP || OP <- Opposition, OP < LastParty, OP > FirstParty],
    Forbidden == [].

-spec is_coalition(cabinet()) -> boolean().
is_coalition(Cab) -> length(Cab) > 1.

% Are any elements in Coalitions subsets of Cabinet?
-spec is_too_large(cabinet(), [cabinet()]) -> boolean().
is_too_large(Cabinet, Coalitions) ->
    Smallers = [ Coalition || Coalition  <- Coalitions,
                              length(Coalition) < length(Cabinet),
                              all_in(Cabinet, Coalition) ],
    length(Smallers) > 0.

% Does this coalition command a majority of seats?
-spec is_winner([cabinet()], [cabinet()]) -> boolean().
is_winner(Coalition, SeatShares) ->
    seat_share(Coalition) > seat_share(SeatShares) / 2.0.

-spec just_party_names([cabinet()]) -> [[atom()]].
just_party_names(Cabs) -> lists:map(fun party_names/1, Cabs).

party_names_min_by(Fun, SeatShares) ->
    Cabs      = mwc_with_seats(SeatShares),
    Vals      = lists:map(Fun, Cabs),
    WithSeats = [ Cab || Cab <- Cabs, Fun(Cab) =:= lists:min(Vals) ],
    just_party_names(WithSeats).

mwc_with_seats(SeatShares) ->
    Winners = winning_coalitions(SeatShares),
    lists:filter(fun(C) -> not is_too_large(C, Winners) end, Winners).

-spec party_endpoints(cabinet()) -> {char(), char()}.
party_endpoints(Cabinet) ->
    [ Hd | Tail ] = lists:sort(party_names(Cabinet)),
    Last = lists:last(Tail),
    {consensus_utils:binary_to_char(Hd), consensus_utils:binary_to_char(Last)}.

-spec party_names(cabinet()) -> [party_name()].
party_names(Cabinet) -> lists:map(fun consensus_party:name/1, Cabinet).

% How many steps between the "leftmost" partner and the "rightmost" partner?
-spec range(cabinet()) -> number().
range(Cabinet) ->
    {Lo, Hi} = party_endpoints(Cabinet),
    Hi - Lo.

% How many seats does this coalition fill?
-spec seat_share([party_result()]) -> number().
seat_share(PartyResults) when is_list(PartyResults) ->
    SumShares = fun(P, Acc) -> consensus_party:seat_share(P) + Acc end,
    lists:foldl(SumShares, 0, PartyResults);
seat_share(PartyResult) -> seat_share([PartyResult]).

-spec winning_coalitions([cabinet()]) -> [cabinet()].
winning_coalitions(SeatShares) ->
    PSet        = consensus_utils:powerset(SeatShares),
    Unique      = consensus_utils:uniqueify(PSet),
    Coalitions  = lists:filter(fun is_coalition/1, Unique),
    lists:filter(fun(C) -> is_winner(C, SeatShares) end, Coalitions).
