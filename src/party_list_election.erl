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
    rankings(jefferson_dhondt, Votes, NumberOfSeats),
    [ {a, 4, 3.4}, {b, 3, 2.8}, {c, 1, 1.1}, {d, 0, 0.7} ].

% Returns {PartyName, ActualSeatsWon, ProportionalSeats}
-spec webster_sainte_lague_rankings([{party_name(), pos_integer()}, ...], pos_integer()) ->
    [{name(), pos_integer(), float()}, ...].
webster_sainte_lague_rankings(Votes, NumberOfSeats) ->
    %%% TODO: real implementation as per
    % https://en.wikipedia.org/wiki/Webster/Sainte-Lagu%C3%AB_method#Description_of_the_method
    rankings(webster_sainte_lague, Votes, NumberOfSeats),
    [ {a, 3, 3.71}, {b, 2, 1.68}, {c, 2, 1.61} ].

%%====================================================================
%% Internal functions
%%====================================================================
quotient(jefferson_dhondt, {_PartyName, SeatsSoFar, VoteCount}) ->
    Denominator = SeatsSoFar + 1,
    VoteCount / Denominator;
quotient(webster_sainte_lague, {_PartyName, SeatsSoFar, VoteCount}) ->
    Denominator = SeatsSoFar*2 + 1,
    VoteCount / Denominator.

rankings(Label, Votes, NumberOfSeatsToFill) ->
    Counters = [ {PartyName, 0, VoteCount} || {PartyName, VoteCount} <- Votes ],
    tabulate(Label, Counters, NumberOfSeatsToFill, 0).

tabulate(_, Counters, SeatsFilled, SeatsFilled) -> Counters;
tabulate(Label, Counters, _SeatsToFill, _SeatsFilled) ->
    [ quotient(Label, C) || C <- Counters ].
