-module(consensus).

%% API exports
-export([
    cabinet_composition/2,
    gallagher_index/1,
    effective_num_parties/1,
    condorcet_rankings/1,
    condorcet_winner/1
]).

-include("parties.hrl").
-include("condorcet.hrl").

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
-spec effective_num_parties([party_result()]) -> number().
effective_num_parties(PartyShares) -> 1 / sum_for(PartyShares).

% Measure of disproportionality proposed by Michael Gallagher.
% Cf. Lijphart, Arend, _Patterns of Democracy_, 1999. pg158. and
% Gallagher, Michael. 1991. "Proportionality, Disproportionality, and
% Electoral systems." _Electoral Studies_ 10, no. 1 (March): 33-51.
-spec gallagher_index([party_result()]) -> number().
gallagher_index(ElectionResults) ->
    consensus_index:gallagher(ElectionResults).

condorcet_rankings(Ballots) ->
    consensus_condorcet:rankings(Ballots).

condorcet_winner(Ballots) ->
    consensus_condorcet:winner(Ballots).

%%====================================================================
%% Internal functions
%%====================================================================

-spec sum_for([party_result()]) -> pos_integer().
sum_for(PartyShares) ->
    lists:foldl(fun sum_seat_share_squares/2, 0, PartyShares).

-spec sum_seat_share_squares(party_result(),
                             pos_integer()) -> pos_integer().
sum_seat_share_squares(PartyResult, Acc) ->
    Seats = consensus_party:seat_share(PartyResult),
    (Seats * Seats) + Acc.

