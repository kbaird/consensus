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
    PartyNames = [ Name || {Name, _Cnt} <- SeatShares ],
    SeatCounts = [ Cnt  || {_Name, Cnt} <- SeatShares ],
    TotalSeats = lists:foldl(fun(Cnt, Sum) -> Cnt + Sum end, 0, SeatCounts),
    trim_mwc(PartyNames, TotalSeats, SeatCounts, []);


compose(policy_viable_coalition, SeatShares) -> compose(pvc, SeatShares);
compose(pvc, _SeatShares) ->
    [].

%%====================================================================
%% Internal functions
%%====================================================================
trim_mwc([], _TotalSeats, _SeatShares, Acc) -> Acc;
trim_mwc([ PartyName | PartyNames ], TotalSeats, SeatShares, Acc) ->
    []. % TODO
