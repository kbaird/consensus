-module(consensus).

%% API exports
-export([
    borda_rankings/2,
    cabinet_composition/2,
    cabinet_composition/3,
    condorcet_rankings/1,
    condorcet_winner/1,
    droop_winners/2,
    effective_num_parties/1,
    gallagher_index/1,
    jefferson_dhondt_rankings/2,
    jefferson_dhondt_rankings/3,
    webster_sainte_lague_rankings/2,
    webster_sainte_lague_rankings/3
]).

%%====================================================================
%% API functions
%%====================================================================

borda_rankings(Label, Ballots) ->
    borda:rankings(Label, Ballots).

% Cf. Lijphart, Arend, _Patterns of Democracy_, 1999. pg93.
cabinet_composition(Label, SeatShares) ->
    consensus_cabinet:compose(Label, SeatShares).

cabinet_composition(Label, SeatShares, AllPartyNames) ->
    consensus_cabinet:compose(Label, SeatShares, AllPartyNames).

condorcet_rankings(Ballots) ->
    condorcet_election:rankings(Ballots).

condorcet_winner(Ballots) ->
    condorcet_election:winner(Ballots).

droop_winners(SeatsCount, Ballots) ->
    droop_election:winners(SeatsCount, Ballots).

% Implement Markku Laakso and Rein Taagepera's index as described in
% Arend Lijphart's Patterns of Democracy (1999), pp67-68. also
% Laakso, Markku and Rein Taagepera. 1979.
% "'Effective' Number of Parties: A Measure with Application to West Europe."
% _Comparative Political Studies_ 12, no. 1 (April): 3-27.
% http://cps.sagepub.com/content/12/1/3.extract
effective_num_parties(PartyShares) ->
    consensus_party:effective_number(PartyShares).

% Measure of disproportionality proposed by Michael Gallagher.
% Cf. Lijphart, Arend, _Patterns of Democracy_, 1999. pg158. and
% Gallagher, Michael. 1991. "Proportionality, Disproportionality, and
% Electoral systems." _Electoral Studies_ 10, no. 1 (March): 33-51.
gallagher_index(ElectionResults) ->
    consensus_gallagher:index(ElectionResults).

jefferson_dhondt_rankings(Votes, NumberOfSeats) ->
    jefferson_dhondt_rankings(Votes, NumberOfSeats, 0.0).

jefferson_dhondt_rankings(Votes, NumberOfSeats, Threshold) ->
    party_list_election:jefferson_dhondt_rankings(Votes, NumberOfSeats, Threshold).

webster_sainte_lague_rankings(Votes, NumberOfSeats) ->
    webster_sainte_lague_rankings(Votes, NumberOfSeats, 0.0).

webster_sainte_lague_rankings(Votes, NumberOfSeats, Threshold) ->
    party_list_election:webster_sainte_lague_rankings(Votes, NumberOfSeats, Threshold).
