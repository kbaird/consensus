-module(schulze_ballot).

%% API exports
-export([
    make/1
]).

-include("schulze.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec make([name(), ...]) -> ballot().
make(CandidateNames) ->
    Candidates = [ schulze_candidate:make(Name) || Name <- CandidateNames ],
    #ballot{candidates = Candidates}.

%%====================================================================
%% Internal functions
%%====================================================================

