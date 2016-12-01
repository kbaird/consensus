-module(condorcet_ballot).

%% API exports
-export([
    make/1
]).

-include("condorcet.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec make([name(), ...]) -> ballot().
make(CandidateNames) ->
    Candidates = lists:map(fun condorcet_candidate:make/1, CandidateNames),
    #ballot{candidates = Candidates}.

%%====================================================================
%% Internal functions
%%====================================================================

