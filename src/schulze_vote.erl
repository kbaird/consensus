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
    Prefs      = preferences(Ballots, #{}),
    Candidates = maps:keys(Prefs),
    select_winner(Candidates, Prefs).

%%====================================================================
%% Internal functions
%%====================================================================
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

-spec add_preferences(candidate(), [], map()) -> map().
add_preferences(_Cand, [], Acc) -> Acc;
add_preferences(Cand,  [ Next | Rest ], AccIn) ->
    Acc1 = increment_vote_count(Cand, Next, AccIn),
    Acc2 = add_preferences(Cand, Rest, Acc1),
    add_preferences(Next, Rest, Acc2).

-spec increment_vote_count(candidate(), [candidate()], map()) -> map().
increment_vote_count(Cand, Next, PrefsIn) ->
    WithCount   = maps:get(Cand, PrefsIn, maps:new()),
    Count       = maps:get(Next, WithCount, 0),
    Incremented = maps:put(Next, Count+1, WithCount),
    maps:put(Cand, Incremented, PrefsIn).

-spec make_candidate(name()) -> candidate().
make_candidate(Name) -> #candidate{name = Name}.

preferences([], Acc)              -> Acc;
preferences([Ballot | Bs], AccIn) ->
    [ Cand | Rest ] = Ballot#ballot.candidates,
    case Rest of
        [] -> maps:put(Cand, winner, maps:new());
        _  -> Acc = add_preferences(Cand, Rest, AccIn),
              preferences(Bs, Acc)
    end.

select_winner([ Cand ], _) -> Cand;
select_winner(Candidates, Prefs) ->
    ByMostVotes = fun(C1,C2) ->
        maps:get(C1, maps:get(C2, Prefs), 0) <
        maps:get(C2, maps:get(C1, Prefs), 0)
    end,
    Sorted = lists:sort(ByMostVotes, Candidates),
    hd(Sorted).
