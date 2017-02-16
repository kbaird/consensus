-module(consensus_condorcet).

%% API exports
-export([
    rankings/1,
    winner/1
]).

-include("parties.hrl").
-include("condorcet.hrl").

%%====================================================================
%% API functions
%%====================================================================

-spec rankings([ballot(), ...]) -> [name(), ...].
rankings(Ballots) ->
    Prefs      = preferences(Ballots, #{}),
    Candidates = maps:keys(Prefs),
    Ranked     = condorcet_candidate:rank(Candidates, Prefs),
    [ C#candidate.name || C <- Ranked ].

-spec winner([ballot(), ...]) -> name().
winner(Ballots) -> hd(rankings(Ballots)).

%%====================================================================
%% Internal functions
%%====================================================================
-spec add_preferences(term(), [term(), ...], term()) -> term().
add_preferences(_Cand, [], Acc) -> Acc;
add_preferences(Cand,  [ Next | Rest ], AccIn) ->
    Acc1 = increment_vote_count(Cand, Next, AccIn),
    Acc2 = add_preferences(Cand, Rest, Acc1),
    add_preferences(Next, Rest, Acc2).

-spec increment_vote_count(candidate(), candidate(), map()) -> map().
increment_vote_count(Cand, Next, PrefsIn) ->
    WithCount   = maps:get(Cand, PrefsIn, maps:new()),
    Count       = maps:get(Next, WithCount, 0),
    Incremented = maps:put(Next, Count+1, WithCount),
    maps:put(Cand, Incremented, PrefsIn).

-spec preferences(list(), map()) -> map().
preferences([],            Acc)   -> Acc;
preferences([Ballot | Bs], AccIn) ->
    [ Cand | Rest ] = Ballot#ballot.candidates,
    case Rest of
        [] -> maps:put(Cand, winner, maps:new());
        _  -> Acc = add_preferences(Cand, Rest, AccIn),
              preferences(Bs, Acc)
    end.
