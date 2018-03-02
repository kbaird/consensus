-module(ballot).

%% API exports
-export([
    make/1,
    candidates/1,
    only_multis/1
]).

-include("elections.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec make([name(), ...]) -> ballot().
make(CandidateNames) ->
    Candidates = lists:map(fun candidate:make/1, CandidateNames),
    #ballot{candidates = Candidates}.

-spec candidates(ballot()) -> [candidate(), ...].
candidates(#ballot{candidates = Candidates}) -> Candidates.

-spec only_multis([ballot()]) -> [ballot()].
only_multis(Ballots) ->
    lists:filter(fun(B) -> length(candidates(B)) > 1 end, Ballots).

%%====================================================================
%% Internal functions
%%====================================================================

