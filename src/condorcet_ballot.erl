-module(condorcet_ballot).

%% API exports
-export([
    make/1,
    candidates/1
]).

-include("elections.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec make([name(), ...]) -> ballot().
make(CandidateNames) ->
    Candidates = lists:map(fun condorcet_candidate:make/1, CandidateNames),
    #ballot{candidates = Candidates}.

-spec candidates(ballot()) -> [candidate(), ...].
candidates(#ballot{candidates = Candidates}) -> Candidates.

%%====================================================================
%% Internal functions
%%====================================================================

