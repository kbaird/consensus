-module(condorcet_candidate).

%% API exports
-export([
    make/1,
    rank/1
]).

-include("condorcet.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec make(name()) -> candidate().
make(Name) -> #candidate{name = Name}.

-spec rank(preferences()) -> [candidate(), ...].
rank(Prefs) ->
    % sort by least votes, so the lowest magnitude for "least votes"
    % (i.e., the winner, with the highest number of votes) is at the front.
    ByLeastVotes = by_least_votes(Prefs),
    Candidates   = maps:keys(Prefs),
    lists:sort(ByLeastVotes, Candidates).

%%====================================================================
%% Internal functions
%%====================================================================
-spec by_least_votes(preferences()) ->
    fun((candidate(), candidate()) -> boolean()).
by_least_votes(Prefs) ->
    % I think this may actually be closer to
    % https://en.wikipedia.org/wiki/Ranked_pairs than Schulze
    % (both satisfy Condorcet criteria)
    fun(C1, C2) ->
        maps:get(C1, maps:get(C2, Prefs), 0) <
        maps:get(C2, maps:get(C1, Prefs), 0)
    end.
