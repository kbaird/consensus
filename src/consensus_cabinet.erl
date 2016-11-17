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
-spec compose(atom(), [{party_name(), seat_share()}, ...]) ->
    [party_name(), ...].
compose(bargaining_proposition, SeatShares) -> compose(bp, SeatShares);
compose(bp, _SeatShares) ->
    [];


compose(minimal_connected_winning, SeatShares) -> compose(mcw, SeatShares);
compose(minimum_connected_winning, SeatShares) -> compose(mcw, SeatShares);
compose(mcw, _SeatShares) ->
    [];


compose(minimal_range, SeatShares) -> compose(mr, SeatShares);
compose(minimum_range, SeatShares) -> compose(mr, SeatShares);
compose(mr, _SeatShares) ->
    [];


compose(minimal_size, SeatShares) -> compose(ms, SeatShares);
compose(minimum_size, SeatShares) -> compose(ms, SeatShares);
compose(ms, SeatShares) ->
    Cabs    = compose(mwc, SeatShares),
    Sizes   = [ length(C) || C <- Cabs ],
    [ MinSize | _ ] = lists:sort(Sizes),
    [ SmallestCab || SmallestCab <- Cabs,
                     length(SmallestCab) =:= MinSize ];


compose(minimal_winning_coalition, SeatShares) -> compose(mwc, SeatShares);
compose(minimum_winning_coalition, SeatShares) -> compose(mwc, SeatShares);
compose(mwc, SeatShares) ->
    PSet       	= powerset(SeatShares),
    Unique      = uniqueify(PSet),
    Coalitions  = lists:filter(fun is_coalition/1, Unique),
    Winners     = lists:filter(fun(C) -> is_winner(C, SeatShares) end, Coalitions),
    WithSeats   = lists:filter(fun(C) -> not too_large(C, PSet) end, Winners),
    [ lists:map(fun({Name, _Cnt}) -> Name end, WithSeat) || WithSeat <- WithSeats ];

compose(policy_viable_coalition, SeatShares) -> compose(pvc, SeatShares);
compose(pvc, _SeatShares) ->
    [].

%%====================================================================
%% Internal functions
%%====================================================================
is_coalition([{_,_}]) -> false;
is_coalition(_)       -> true.

is_winner(Coalition, SeatShares) ->
    share(Coalition) > share(SeatShares) / 2.0.

powerset([]) -> [[]];
powerset([H|T]) ->
    PT = powerset(T),
    powerset(H, PT, PT).

powerset(_, [],    Acc) -> Acc;
powerset(X, [H|T], Acc) ->
    powerset(X, T, [[X|H]|Acc]).

share({_Name, Cnt}) -> Cnt;
share(L) ->
    lists:foldl(fun({_N, Cnt}, Sum) -> Cnt + Sum end, 0, L).

too_large(C, _Perms) ->
    length(C) > 3.
    %InC = fun(Party) -> lists:member(Party, C) end,
    %lists:any([ Perm || Perm    <- Perms,
    %                    Parties <- Perm,
    %                    length(Parties) < length(C),
    %                    lists:all(InC, Parties) ]).

uniqueify(Ls) ->
    lists:usort([ lists:usort(L) || L <- Ls ]).
