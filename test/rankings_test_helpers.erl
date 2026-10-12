-module(rankings_test_helpers).
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

-export([assert_default_threshold_matches/4, assert_seats/3, seats_by_name/1]).

% Asserts that the two-argument form of an allocation function agrees with the
% three-argument form at the explicit 0.0 default threshold.
assert_default_threshold_matches(Votes, NumberOfSeats, DefaultThresholdRankings, ExplicitThresholdRankings) ->
    ?assertEqual(
        ExplicitThresholdRankings(Votes, NumberOfSeats, 0.0),
        DefaultThresholdRankings(Votes, NumberOfSeats)).

% Maps {PartyName, SeatsWon, _ExactSeats} rankings to a SeatsByName map.
seats_by_name(Rankings) ->
    maps:from_list([{Party, Seats} || {Party, Seats, _} <- Rankings]).

% Asserts the allocated total against SeatsRenewed and each expected party's
% seat count against the rankings-derived SeatsByName map.
assert_seats(Rankings, SeatsRenewed, ExpectedSeatsByName) ->
    SeatsByName = seats_by_name(Rankings),
    ?assertEqual(SeatsRenewed, lists:sum(maps:values(SeatsByName))),
    lists:foreach(
        fun({Party, ExpectedSeats}) ->
            ?assertEqual(ExpectedSeats, maps:get(Party, SeatsByName))
        end,
        maps:to_list(ExpectedSeatsByName)).