-module(jefferson_dhondt_election).

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
    % https://en.wikipedia.org/wiki/D%27Hondt_method#Allocation
    [ {a, 4, 3.4}, {b, 3, 2.8}, {c, 1, 1.1}, {d, 0, 0.7} ].

%%====================================================================
%% Internal functions
%%====================================================================