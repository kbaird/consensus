-module(consensus).

%% API exports
-export([
    cabinet_composition/2,
    gallagher_index/1,
    effective_num_parties/1,
    schulze_rankings/1,
    schulze_winner/1
]).

-include("parties.hrl").
-include("schulze.hrl").

%%====================================================================
%% API functions
%%====================================================================

% Cf. Lijphart, Arend, _Patterns of Democracy_, 1999. pg93.
cabinet_composition(Label, SeatShares) ->
    consensus_cabinet:compose(Label, SeatShares).

% Implement Markku Laakso and Rein Taagepera's index as described in
% Arend Lijphart's Patterns of Democracy (1999), pp67-68.

% Laakso, Markku and Rein Taagepera. 1979.
% "'Effective' Number of Parties: A Measure with Application to West Europe."
% _Comparative Political Studies_ 12, no. 1 (April): 3-27.
% http://cps.sagepub.com/content/12/1/3.extract
-spec effective_num_parties([{party_name(), seat_share()}, ...]) -> number().
effective_num_parties(PartyShares) -> 1 / sum_for(PartyShares).

-spec gallagher_index([{party_name(), seat_share(), vote_share()}, ...]) -> number().
gallagher_index(ElectionResults) ->
    consensus_index:gallagher(ElectionResults).

-spec schulze_rankings([ballot(), ...]) -> [name(), ...].
schulze_rankings(Ballots) ->
    consensus_schulze:rankings(Ballots).

-spec schulze_winner([ballot(), ...]) -> name().
schulze_winner(Ballots) ->
    consensus_schulze:winner(Ballots).

%%====================================================================
%% Internal functions
%%====================================================================

sum_for(PartyShares) ->
    lists:foldl(fun sum_share_squares/2, 0, PartyShares).

sum_share_squares({_, Share}, Sum) -> (Share * Share) + Sum.

