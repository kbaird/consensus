-module(party_list_election).

%% API exports
-export([
    jefferson_dhondt_rankings/3,
    jefferson_dhondt_rankings/4,
    webster_sainte_lague_rankings/3
]).

-include("include/elections.hrl").
-include("include/parties.hrl").

-define(STARTING_SEATS, 0).

%%====================================================================
%% API functions
%%====================================================================

% Returns {PartyName, ActualSeatsWon, ExactSeats}
-spec jefferson_dhondt_rankings([{party_name(), pos_integer()}, ...], pos_integer(), float()) ->
    [{party_name(), pos_integer(), float()}, ...].
jefferson_dhondt_rankings(Votes, NumberOfSeats, Threshold) ->
    jefferson_dhondt_rankings(Votes, NumberOfSeats, Threshold, #{}).

% Capping a party at MaxSeatsByName models its inability to fill every seat
% it would otherwise win (e.g. too few list candidates); a seat a capped
% party cannot take passes to the next-highest quotient.
% Returns {PartyName, ActualSeatsWon, ExactSeats}
-spec jefferson_dhondt_rankings([{party_name(), pos_integer()}, ...], pos_integer(), float(),
                                #{party_name() => non_neg_integer()}) ->
    [{party_name(), pos_integer(), float()}, ...].
jefferson_dhondt_rankings(Votes, NumberOfSeats, Threshold, MaxSeatsByName) ->
    % https://en.wikipedia.org/wiki/D%27Hondt_method#Allocation
    rankings(jefferson_dhondt, Votes, NumberOfSeats, Threshold, MaxSeatsByName).

% Returns {PartyName, ActualSeatsWon, ExactSeats}
-spec webster_sainte_lague_rankings([{party_name(), pos_integer()}, ...], pos_integer(), float()) ->
    [{party_name(), pos_integer(), float()}, ...].
webster_sainte_lague_rankings(Votes, NumberOfSeats, Threshold) ->
    % https://en.wikipedia.org/wiki/Webster/Sainte-Lagu%C3%AB_method#Description_of_the_method
    rankings(webster_sainte_lague, Votes, NumberOfSeats, Threshold, #{}).

%%====================================================================
%% Internal functions
%%====================================================================
append_quotients(Method, Counters) ->
    [
        {PartyName, SeatsSoFar, VoteCnt, quotient(Method, SeatsSoFar, VoteCnt)} ||
        {PartyName, SeatsSoFar, VoteCnt} <- Counters
    ].

by_highest_quotient({_, _, _, Q1}, {_, _, _, Q2}) -> Q1 > Q2.

by_seats_won({_, S1, _}, {_, S2, _}) -> S1 > S2.

counters_for_parties_with_enough_votes(Votes, MinVoteCnt) ->
    [ {PartyName, ?STARTING_SEATS, VoteCount} ||
      {PartyName, VoteCount} <- Votes, VoteCount >= MinVoteCnt ].

quotient(jefferson_dhondt, SeatsSoFar, VoteCount) ->
    Denominator = SeatsSoFar + 1,
    VoteCount / Denominator;

quotient(webster_sainte_lague, SeatsSoFar, VoteCount) ->
    Denominator = SeatsSoFar * 2 + 1,
    VoteCount / Denominator.

rankings(Method, Votes, NumberOfSeatsToFill, Threshold, MaxSeatsByName) ->
    TotalVoteCnt = lists:sum([ VoteCount || {_PartyName, VoteCount} <- Votes ]),
    MinVoteCnt   = TotalVoteCnt * Threshold,
    Counters     = counters_for_parties_with_enough_votes(Votes, MinVoteCnt),
    tabulate(Method, Counters, TotalVoteCnt, NumberOfSeatsToFill, ?STARTING_SEATS, MaxSeatsByName).

% Theoretical number of seats before rounding
share(Votes, TotalVotes, SeatsFilled) ->
    trunc(Votes * SeatsFilled * 100.0 / TotalVotes) / 100.0.

seats_cap(PartyName, TotalSeats, MaxSeatsByName) ->
    maps:get(PartyName, MaxSeatsByName, TotalSeats).

increment_counter(PartyName, {PartyName, SeatsSoFar, VoteCount, Quotient}) ->
    {PartyName, SeatsSoFar + 1, VoteCount, Quotient};
increment_counter(_OtherPartyName, Counter) ->
    Counter.

tabulate(_Method, Counters, TotalVoteCnt, TotalSeats, TotalSeats, _MaxSeatsByName) ->
    Sorted = lists:sort(fun by_seats_won/2, Counters),
    [ {PtyName, PtySeats, share(VoteCount, TotalVoteCnt, TotalSeats)} ||
      {PtyName, PtySeats, VoteCount} <- Sorted ];

tabulate(Method, Counters, TotalVoteCnt, TotalSeats, SeatsFilled, MaxSeatsByName)
        when SeatsFilled < TotalSeats ->
    CountersWithQ = append_quotients(Method, Counters),
    EligibleWithQ = [ Counter || Counter = {PtyName, SeatsSoFar, _, _} <- CountersWithQ,
                                 SeatsSoFar < seats_cap(PtyName, TotalSeats, MaxSeatsByName) ],
    case EligibleWithQ of
        [] ->
            tabulate(Method, Counters, TotalVoteCnt, TotalSeats, TotalSeats, MaxSeatsByName);
        _  ->
            [ {WinPN, _, _, _} | _ ] = lists:sort(fun by_highest_quotient/2, EligibleWithQ),
            SortedWithQ         = lists:sort(fun by_highest_quotient/2, CountersWithQ),
            IncrementedCounters = [ increment_counter(WinPN, Counter) || Counter <- SortedWithQ ],
            CountersWithoutQ    = [ {PN, S, VC} || {PN, S, VC, _Q} <- IncrementedCounters ],
            tabulate(Method, CountersWithoutQ, TotalVoteCnt, TotalSeats,
                     SeatsFilled+1, MaxSeatsByName)
    end.
