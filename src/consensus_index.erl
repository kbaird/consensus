-module(consensus_index).

%% API exports
-export([
    gallagher/1
]).

-include("parties.hrl").
-include("condorcet.hrl").

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

-spec seat_totals_to_percentages([any()]) -> [consensus_party:party_result()].
seat_totals_to_percentages(SeatTotals) ->
    TotalSeats = lists:foldl(fun sum_seats/2, 0, SeatTotals),
    [ consensus_party:make(Name, Seats/TotalSeats, V) ||
        #party_result{name       = Name,
                      seat_share = Seats,
                      vote_share = V} <- SeatTotals ].

-spec sum_diff_squares(party_result(), pos_integer()) -> pos_integer().
sum_diff_squares(#party_result{seat_share = SeatPC,
                               vote_share = VotePC}, Sum) ->
    ((VotePC - SeatPC) * (VotePC - SeatPC)) + Sum.

-spec sum_seats(party_result(), pos_integer()) -> pos_integer().
sum_seats(PartyResult, Sum) ->
    Seats = consensus_party:seat_share(PartyResult),
    Seats + Sum.

-spec sum_squares_of_pc_diffs([consensus_party:party_result()]) -> number().
sum_squares_of_pc_diffs(ElectionResults) ->
    lists:foldl(fun sum_diff_squares/2, 0, ElectionResults).
