-module(schulze_vote).

%% API exports
-export([
    make_ballot/1,
    winner/1
]).

-include("schulze.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec make_ballot([candidate(), ...]) -> ballot().
make_ballot(Ballot) -> Ballot.

-spec winner([ballot(), ...]) -> candidate().
winner([Candidate | _]) -> hd(Candidate).

%%====================================================================
%% Internal functions
%%====================================================================
