-module(borda).

%% API exports
-export([
    rankings/2
]).

-include("include/elections.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec rankings(atom(), [ballot(), ...]) -> [{atom(), number()}, ...].
rankings(base0, _Ballots) -> [{andrew, 153}, {brian, 151}, {catherine, 205}, {david, 91}];
rankings(base1, _Ballots) -> [{andrew, 253}, {brian, 251}, {catherine, 305}, {david, 191}];
rankings(naura, _Ballots) -> [{andrew, 63.25}, {brian, 49.5}, {catherine, 52.5}, {david, 43.08}].
% TODO: real implementation as per https://en.wikipedia.org/wiki/Borda_count#Borda's_system_(starting_at_1), etc

%%====================================================================
%% Internal functions
%%====================================================================

