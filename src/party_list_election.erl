-module(party_list_election).

%% API exports
-export([
    jefferson_dhondt_rankings/2,
    webster_sainte_lague_rankings/2
]).

-include("include/elections.hrl").

%%====================================================================
%% API functions
%%====================================================================

% Returns {PartyName, ActualSeatsWon, ProportionalSeats}
-spec jefferson_dhondt_rankings([ballot(), ...], pos_integer()) ->
    [{name(), pos_integer(), float()}, ...].
jefferson_dhondt_rankings(_Ballots, _NumberOfSeats) ->
    %%% TODO: real implementation as per
    % https://en.wikipedia.org/wiki/D%27Hondt_method#Allocation
    [ {a, 4, 3.4}, {b, 3, 2.8}, {c, 1, 1.1}, {d, 0, 0.7} ].

% Returns {PartyName, ActualSeatsWon, ProportionalSeats}
-spec webster_sainte_lague_rankings([ballot(), ...], pos_integer()) ->
    [{name(), pos_integer(), float()}, ...].
webster_sainte_lague_rankings(_Ballots, _NumberOfSeats) ->
    %%% TODO: real implementation as per
    % https://en.wikipedia.org/wiki/Webster/Sainte-Lagu%C3%AB_method#Description_of_the_method
    [ {a, 3, 3.71}, {b, 2, 1.68}, {c, 2, 1.61} ].

%%====================================================================
%% Internal functions
%%====================================================================
