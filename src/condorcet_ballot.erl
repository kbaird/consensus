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
    Candidates = [ condorcet_candidate:make(Name) || Name <- CandidateNames ],
    #ballot{candidates = Candidates}.

%%====================================================================
%% Internal functions
%%====================================================================

