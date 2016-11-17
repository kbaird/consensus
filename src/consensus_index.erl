-module(consensus_index).

%% API exports
-export([
    gallagher/1
]).

-include("parties.hrl").
-include("schulze.hrl").

%%====================================================================
%% API functions
%%====================================================================

-spec gallagher([{party_name(), seat_share(), vote_share()}, ...]) -> number().
gallagher(ElectionResults) ->
    PCs = seat_totals_to_percentages(ElectionResults),
    Sum = sum_squares_of_pc_diffs(PCs),
    math:sqrt(Sum / 2).
    % G = sqrt(1/2 * sum( (vote_pc - seat_pc) ** 2 ))

%%====================================================================
%% Internal functions
%%====================================================================

seat_totals_to_percentages(SeatTotals) ->
    TotalSeats = lists:foldl(fun sum_seats/2, 0, SeatTotals),
    [ {Name, Seats/TotalSeats, V} || {Name, Seats, V} <- SeatTotals ].

sum_diff_squares({_, SeatPC, VotePC}, Sum) ->
    ((VotePC - SeatPC) * (VotePC - SeatPC)) + Sum.

sum_seats({_, Seats, _}, Sum) -> Seats + Sum.

sum_squares_of_pc_diffs(ElectionResults) ->
    lists:foldl(fun sum_diff_squares/2, 0, ElectionResults).
