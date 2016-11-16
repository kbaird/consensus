-module(consensus).

%% API exports
-export([
    effective_num_parties/1,
    schulze_rankings/1,
    schulze_winner/1
]).

-include("parties.hrl").
-include("schulze.hrl").

%%====================================================================
%% API functions
%%====================================================================

% Implement Markku Laakso and Rein Taagepera's index as described in
% Arend Lijphart's Patterns of Democracy (1999), pp67-68.

% Laakso, Markku and Rein Taagepera. 1979.
% "'Effective' Number of Parties: A Measure with Application to West Europe."
% _Comparative Political Studies_ 12, no. 1 (April): 3-27.
% http://cps.sagepub.com/content/12/1/3.extract
-spec effective_num_parties([{party_name(), seat_share()}, ...]) -> number().
effective_num_parties(PartyShares) -> 1 / sum_for(PartyShares).

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

sum_for(PartyShares) ->
    lists:foldl(fun sum_share_squares/2, 0, PartyShares).

sum_share_squares({_, Share}, Sum) -> (Share * Share) + Sum.
