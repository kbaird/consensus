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
    Perms       = permutations(SeatShares),
    Coalitions  = lists:filter(fun is_coalition/1, Perms),
    Winners     = lists:filter(fun(C) -> is_winner(C, SeatShares) end, Coalitions),
    lists:filter(fun(C) -> not too_large(C, Perms) end, Winners).

compose(policy_viable_coalition, SeatShares) -> compose(pvc, SeatShares);
compose(pvc, _SeatShares) ->
    [].

%%====================================================================
%% Internal functions
%%====================================================================
is_coalition(Cand) -> length(Cand) > 1.

is_winner(Coalition, SeatShares) ->
    share(Coalition) > share(SeatShares) / 2.0.

permutations([]) -> [[]];
permutations(L)  ->
    [[H|T] || H <- L, T <- permutations(L--[H])].

share(L) ->
    lists:foldl(fun({_N, Cnt}, Sum) -> Cnt + Sum end, 0, L).

too_large(C, Perms) ->
    InC = fun(Party) -> lists:member(Party, C) end,
    lists:any([ Perm || Perm    <- Perms,
                        Parties <- Perm,
                        length(Parties) < length(C),
                        lists:all(InC, Parties) ]).
