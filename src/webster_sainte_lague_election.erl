-module(webster_sainte_lague_election).

%% API exports
-export([
    rankings/2
]).

-include("include/elections.hrl").

%%====================================================================
%% API functions
%%====================================================================

% Returns {PartyName, ActualSeatsWon, ProportionalSeats}
-spec rankings([ballot(), ...], pos_integer()) -> [{name(), pos_integer(), float()}, ...].
rankings(_Ballots, _NumberOfSeats) ->
    %%% TODO: real implementation as per
    % https://en.wikipedia.org/wiki/Webster/Sainte-Lagu%C3%AB_method#Description_of_the_method
    [ {a, 3, 3.71}, {b, 2, 1.68}, {c, 2, 1.61} ].

%%====================================================================
%% Internal functions
%%====================================================================