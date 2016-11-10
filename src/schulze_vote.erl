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
-spec make_preferences([ballot(), ...]) -> map().
make_preferences(Ballots) ->
% Input: d[i,j], the number of voters who prefer candidate i to candidate j.
% Output: p[i,j], the strength of the strongest path from candidate i to candidate j.

% for i from 1 to C
%    for j from 1 to C
%       if (i ≠ j) then
%          if (d[i,j] > d[j,i]) then
%             p[i,j] := d[i,j]
%          else
%             p[i,j] := 0
%
% for i from 1 to C
%    for j from 1 to C
%       if (i ≠ j) then
%          for k from 1 to C
%             if (i ≠ k and j ≠ k) then
%                p[j,k] := max ( p[j,k], min ( p[j,i], p[i,k] ) )
    #{}.

-spec make_candidate(name()) -> candidate().
make_candidate(Name) -> #candidate{name = Name}.

most_votes({_, Xcnt}, {_, Ycnt}) -> Xcnt >= Ycnt.

% TODO: implement https://en.wikipedia.org/wiki/Schulze_method#Computation instead
winner([], Acc) when is_map(Acc) -> winner([], maps:to_list(Acc));
winner([], Acc)                  -> hd(lists:sort(fun most_votes/2, Acc));
winner([Ballot | Bs], Acc) ->
    Candidate = hd(Ballot#ballot.candidates),
    Count     = maps:get(Candidate, Acc, 0),
    winner(Bs, maps:put(Candidate, Count+1, Acc)).
