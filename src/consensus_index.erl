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

-spec gallagher([party_result(), ...]) -> number().
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
    [ consensus_party:make(Name, Seats/TotalSeats, V) ||
        #party_result{name       = Name,
                      seat_share = Seats,
                      vote_share = V} <- SeatTotals ].

sum_diff_squares(#party_result{seat_share = SeatPC,
                               vote_share = VotePC}, Sum) ->
    ((VotePC - SeatPC) * (VotePC - SeatPC)) + Sum.

sum_seats(#party_result{seat_share = Seats}, Sum) -> Seats + Sum.

sum_squares_of_pc_diffs(ElectionResults) ->
    lists:foldl(fun sum_diff_squares/2, 0, ElectionResults).
