-module(consensus).

%% API exports
-export([
    schulze_rankings/1,
    schulze_winner/1
]).

-include("schulze.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec schulze_rankings([ballot(), ...]) -> [name(), ...].
schulze_rankings(Ballots) ->
    Prefs      = preferences(Ballots, #{}),
    Candidates = maps:keys(Prefs),
    Ranked     = schulze_candidate:rank(Candidates, Prefs),
    [ C#candidate.name || C <- Ranked ].

-spec schulze_winner([ballot(), ...]) -> name().
schulze_winner(Ballots) -> hd(schulze_rankings(Ballots)).

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

-spec preferences(list(), map()) -> map().
preferences([], Acc)              -> Acc;
preferences([Ballot | Bs], AccIn) ->
    [ Cand | Rest ] = Ballot#ballot.candidates,
    case Rest of
        [] -> maps:put(Cand, winner, maps:new());
        _  -> Acc = add_preferences(Cand, Rest, AccIn),
              preferences(Bs, Acc)
    end.
