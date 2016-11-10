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

-spec winner([ballot(), ...]) -> name().
winner(Ballots) ->
    Prefs      = preferences(Ballots, #{}),
    Candidates = maps:keys(Prefs),
    Winner     = select_winner(Candidates, Prefs),
    Winner#candidate.name.

%%====================================================================
%% Internal functions
%%====================================================================

add_preferences(_Cand, [], Acc) -> Acc;
add_preferences(Cand,  [ Next | Rest ], AccIn) ->
    Acc1 = increment_vote_count(Cand, Next, AccIn),
    Acc2 = add_preferences(Cand, Rest, Acc1),
    add_preferences(Next, Rest, Acc2).

increment_vote_count(Cand, Next, PrefsIn) ->
    WithCount   = maps:get(Cand, PrefsIn, maps:new()),
    Count       = maps:get(Next, WithCount, 0),
    Incremented = maps:put(Next, Count+1, WithCount),
    maps:put(Cand, Incremented, PrefsIn).

-spec make_candidate(name()) -> candidate().
make_candidate(Name) -> #candidate{name = Name}.

-spec preferences(list(), map()) -> map().
preferences([], Acc)              -> Acc;
preferences([Ballot | Bs], AccIn) ->
    [ Cand | Rest ] = Ballot#ballot.candidates,
    case Rest of
        [] -> maps:put(Cand, winner, maps:new());
        _  -> Acc = add_preferences(Cand, Rest, AccIn),
              preferences(Bs, Acc)
    end.

-spec select_winner(list(), map()) -> candidate().
select_winner([ Cand ], _) -> Cand;
select_winner(Candidates, Prefs) ->
    ByLeastVotes = fun(C1, C2) ->
        maps:get(C1, maps:get(C2, Prefs), 0) <
        maps:get(C2, maps:get(C1, Prefs), 0)
    end,
    % sort by least votes, so the lowest magnitude for "least votes"
    % (i.e., the winner, with the highest number of votes) is at the front.
    Sorted = lists:sort(ByLeastVotes, Candidates),
    hd(Sorted).
