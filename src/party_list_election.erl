-module(party_list_election).

%% API exports
-export([
    jefferson_dhondt_rankings/2,
    webster_sainte_lague_rankings/2
]).

-include("include/elections.hrl").
-include("include/parties.hrl").

%%====================================================================
%% API functions
%%====================================================================

% Returns {PartyName, ActualSeatsWon, ProportionalSeats}
-spec jefferson_dhondt_rankings([{party_name(), pos_integer()}, ...], pos_integer()) ->
    [{name(), pos_integer(), float()}, ...].
jefferson_dhondt_rankings(Votes, NumberOfSeats) ->
    %%% TODO: real implementation as per
    % https://en.wikipedia.org/wiki/D%27Hondt_method#Allocation
    rankings(jefferson_dhondt, Votes, NumberOfSeats).

% Returns {PartyName, ActualSeatsWon, ProportionalSeats}
-spec webster_sainte_lague_rankings([{party_name(), pos_integer()}, ...], pos_integer()) ->
    [{name(), pos_integer(), float()}, ...].
webster_sainte_lague_rankings(Votes, NumberOfSeats) ->
    %%% TODO: real implementation as per
    % https://en.wikipedia.org/wiki/Webster/Sainte-Lagu%C3%AB_method#Description_of_the_method
    rankings(webster_sainte_lague, Votes, NumberOfSeats).

%%====================================================================
%% Internal functions
%%====================================================================
by_highest_quotient({_, _, _, Q1}, {_, _, _, Q2}) -> Q1 > Q2.

by_seats_won({_, S1, _}, {_, S2, _}) -> S1 > S2.

quotient(jefferson_dhondt, SeatsSoFar, VoteCount) ->
    Denominator = SeatsSoFar + 1,
    VoteCount / Denominator;

quotient(webster_sainte_lague, SeatsSoFar, VoteCount) ->
    Denominator = SeatsSoFar*2 + 1,
    VoteCount / Denominator.

rankings(Label, Votes, NumberOfSeatsToFill) ->
    Counters = [ {PartyName, 0, VoteCount} || {PartyName, VoteCount} <- Votes ],
    tabulate(Label, Counters, NumberOfSeatsToFill, 0).

share(Votes, TotalVotes, SeatsFilled) ->
    Prec = math:pow(10, 2),
    trunc(Votes /TotalVotes * SeatsFilled * Prec) / Prec.

tabulate(_, Counters, SeatsFilled, SeatsFilled) ->
    VoteCounts = [ VoteCount || {_PN, _SSF, VoteCount} <- Counters ],
    TotalVotes = lists:sum(VoteCounts),
    Sorted     = lists:sort(fun by_seats_won/2, Counters),
    [ {PN, PartySeats, share(VoteCount, TotalVotes, SeatsFilled)} || {PN, PartySeats, VoteCount} <- Sorted ];

tabulate(Label, Counters, SeatsToFill, SeatsFilled) ->
    Counters2 = [ {PartyName, SeatsSoFar, VoteCount, quotient(Label, SeatsSoFar, VoteCount)} ||
                  {PartyName, SeatsSoFar, VoteCount} <- Counters ],
    [ {WinPN, WinSeatsSoFar, WinVC, WinQ} | Rest ] = lists:sort(fun by_highest_quotient/2, Counters2),
    Counters3 = [ {WinPN, WinSeatsSoFar+1, WinVC, WinQ} | Rest ],
    Counters4 = [ {PartyName, SeatsSoFar, VoteCount} || {PartyName, SeatsSoFar, VoteCount, _Q} <- Counters3 ],
    tabulate(Label, Counters4, SeatsToFill, SeatsFilled+1).
