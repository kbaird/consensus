-module(schulze_candidate).

%% API exports
-export([
    make/1,
    rank/2
]).

-include("schulze.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec make(name()) -> candidate().
make(Name) -> #candidate{name = Name}.

-spec rank(list(), map()) -> [candidate(), ...].
rank(Candidates, Prefs) ->
    ByLeastVotes = fun(C1, C2) ->
        maps:get(C1, maps:get(C2, Prefs), 0) <
        maps:get(C2, maps:get(C1, Prefs), 0)
    end,
    % sort by least votes, so the lowest magnitude for "least votes"
    % (i.e., the winner, with the highest number of votes) is at the front.
    lists:sort(ByLeastVotes, Candidates).

%%====================================================================
%% Internal functions
%%====================================================================

