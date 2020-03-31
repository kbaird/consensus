-module(consensus_gallagher).

%% API exports
-export([
    index/1
]).

-include("include/parties.hrl").

%%====================================================================
%% API functions
%%====================================================================

% See consensus:gallagher_index for references
-spec index([party_result(), ...]) -> number().
index(ElectionResults) ->
    % Cf. Lijphart, Arend, _Patterns of Democracy_, 1999. pg158.
    % measure of disproportionality:
    %   G = √(1/2 * Σ(Vi - Si)^2)
    %       where Vi is vote % at index i
    %       and Si is seat % at index i
    PCs = results_to_percentages(ElectionResults),
    Sum = lists:foldl(fun sum_diff_squares/2, 0, PCs),
    math:sqrt(Sum / 2).

%%====================================================================
%% Internal functions
%%====================================================================

-spec express_seat_share_as_percentage(party_result(), number()) -> party_result().
express_seat_share_as_percentage(PartyResult, TotalSeats) ->
    Name  = consensus_party:name(PartyResult),
    Seats = consensus_party:seat_share(PartyResult),
    Votes = consensus_party:vote_share(PartyResult),
    consensus_party:make(Name, Seats/TotalSeats, Votes).

-spec results_to_percentages([party_result(), ...]) -> [party_result(), ...].
results_to_percentages(ElectionResults) ->
    TotalSeats = lists:foldl(fun sum_seats/2, 0, ElectionResults),
    [ express_seat_share_as_percentage(PartyResult, TotalSeats) ||
      PartyResult <- ElectionResults ].

-spec sum_diff_squares(party_result(), pos_integer()) -> pos_integer().
sum_diff_squares(PartyResult, Acc) ->
    SeatPC = consensus_party:seat_share(PartyResult),
    VotePC = consensus_party:vote_share(PartyResult),
    ((VotePC - SeatPC) * (VotePC - SeatPC)) + Acc.

-spec sum_seats(party_result(), pos_integer()) -> pos_integer().
sum_seats(PartyResult, Acc) ->
    Seats = consensus_party:seat_share(PartyResult),
    Seats + Acc.
