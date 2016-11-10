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
-spec make_ballot([name(), ...]) -> ballot().
make_ballot(CandidateNames) ->
    Candidates = [ make_candidate(Name) || Name <- CandidateNames ],
    #ballot{candidates = Candidates}.

-spec winner([ballot(), ...]) -> candidate().
winner(Ballots) ->
    {Candidate, _} = winner(Ballots, #{}),
    Candidate.

%%====================================================================
%% Internal functions
%%====================================================================
-spec make_candidate(name()) -> candidate().
make_candidate(Name) -> #candidate{name = Name}.

most_votes({_, Xcnt}, {_, Ycnt}) -> Xcnt >= Ycnt.

winner([], Acc) when is_map(Acc) -> winner([], maps:to_list(Acc));
winner([], Acc)                  -> hd(lists:sort(fun most_votes/2, Acc));
winner([Ballot | Bs], Acc) ->
    Candidate = hd(Ballot#ballot.candidates),
    Count     = maps:get(Candidate, Acc, 0),
    winner(Bs, maps:put(Candidate, Count+1, Acc)).
