-module(consensus_index).

%% API exports
-export([
    gallagher/1
]).

-include("parties.hrl").

%%====================================================================
%% API functions
%%====================================================================

-spec gallagher([party_result(), ...]) -> number().
gallagher(ElectionResults) ->
    PCs = results_to_percentages(ElectionResults),
    Sum = sum_squares_of_pc_diffs(PCs),
    math:sqrt(Sum / 2).
    % G = sqrt(1/2 * sum( (vote_pc - seat_pc) ** 2 ))

%%====================================================================
%% Internal functions
%%====================================================================

express_seat_share_as_percentage(PartyResult, TotalSeats) ->
    Name  = consensus_party:name(PartyResult),
    Seats = consensus_party:seat_share(PartyResult),
    Votes = consensus_party:vote_share(PartyResult),
    consensus_party:make(Name, Seats/TotalSeats, Votes).

-spec results_to_percentages([any()]) -> [consensus_party:party_result()].
results_to_percentages(ElectionResults) ->
    TotalSeats = lists:foldl(fun sum_seats/2, 0, ElectionResults),
    [ express_seat_share_as_percentage(PartyResult, TotalSeats) ||
      PartyResult <- ElectionResults ].

-spec sum_diff_squares(party_result(), pos_integer()) -> pos_integer().
sum_diff_squares(PartyResult, Sum) ->
    SeatPC = consensus_party:seat_share(PartyResult),
    VotePC = consensus_party:vote_share(PartyResult),
    ((VotePC - SeatPC) * (VotePC - SeatPC)) + Sum.

-spec sum_seats(party_result(), pos_integer()) -> pos_integer().
sum_seats(PartyResult, Sum) ->
    Seats = consensus_party:seat_share(PartyResult),
    Seats + Sum.

-spec sum_squares_of_pc_diffs([consensus_party:party_result()]) -> number().
sum_squares_of_pc_diffs(ElectionResults) ->
    lists:foldl(fun sum_diff_squares/2, 0, ElectionResults).
