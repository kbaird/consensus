-module(party_list_election).

%% API exports
-export([
    jefferson_dhondt_rankings/3,
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
    % https://en.wikipedia.org/wiki/D%27Hondt_method#Allocation
    rankings(jefferson_dhondt, Votes, NumberOfSeats, Threshold).

% Returns {PartyName, ActualSeatsWon, ExactSeats}
-spec webster_sainte_lague_rankings([{party_name(), pos_integer()}, ...], pos_integer(), float()) ->
    [{party_name(), pos_integer(), float()}, ...].
webster_sainte_lague_rankings(Votes, NumberOfSeats, Threshold) ->
    % https://en.wikipedia.org/wiki/Webster/Sainte-Lagu%C3%AB_method#Description_of_the_method
    rankings(webster_sainte_lague, Votes, NumberOfSeats, Threshold).

%%====================================================================
%% Internal functions
%%====================================================================
add_quotients(Method, Counters) ->
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

rankings(Method, Votes, NumberOfSeatsToFill, Threshold) ->
    TotalVoteCnt = lists:sum([ VoteCount || {_PartyName, VoteCount} <- Votes ]),
    MinVoteCnt   = TotalVoteCnt * Threshold,
    Counters     = counters_for_parties_with_enough_votes(Votes, MinVoteCnt),
    tabulate(Method, Counters, TotalVoteCnt, NumberOfSeatsToFill, ?STARTING_SEATS).

remove_quotients(CountersWithQ) ->
    [ {PartyName, SeatsSoFar, VoteCount} ||
      {PartyName, SeatsSoFar, VoteCount, _Q} <- CountersWithQ ].

share(Votes, TotalVotes, SeatsFilled) ->
    Prec = math:pow(10, 2),
    trunc(Votes * SeatsFilled * Prec / TotalVotes) / Prec.

tabulate(_Method, Counters, TotalVoteCnt, TotalSeats, TotalSeats) ->
    Sorted = lists:sort(fun by_seats_won/2, Counters),
    [ {PtyName, PtySeats, share(VoteCount, TotalVoteCnt, TotalSeats)} ||
      {PtyName, PtySeats, VoteCount} <- Sorted ];

tabulate(Method, Counters, TotalVoteCnt, TotalSeats, SeatsFilled) when SeatsFilled < TotalSeats ->
    CountersWithQ = add_quotients(Method, Counters),
    [ Winner | Rest ] = lists:sort(fun by_highest_quotient/2, CountersWithQ),
    {WinPN, WinSeatsSoFar, WinVC, WinQ} = Winner,
    IncrementedCounters = [ {WinPN, WinSeatsSoFar+1, WinVC, WinQ} | Rest ],
    CountersWithoutQ    = remove_quotients(IncrementedCounters),
    tabulate(Method, CountersWithoutQ, TotalVoteCnt, TotalSeats, SeatsFilled+1).
