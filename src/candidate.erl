-module(candidate).

%% API exports
-export([
    make/1,
    name/1,
    party/1,
    rank/1
]).

-include("include/elections.hrl").
-include("include/parties.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec make(name() | {name(), party_name()}) -> candidate().
make({Name, Party}) when is_atom(Name) andalso is_atom(Party) ->
    #candidate{name = Name, party = Party};
make(Name) when is_atom(Name) -> #candidate{name = Name}.

-spec name(candidate()) -> name().
name(#candidate{name = Name}) -> Name.

-spec party(candidate()) -> atom().
party(#candidate{party = Party}) -> Party.

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
