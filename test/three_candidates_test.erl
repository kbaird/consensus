-module('three_candidates_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

three_candidates_test_() ->
    [
        fun three_candidates_one_voter_case/0,
        fun three_candidates_majority_case/0,
        fun three_candidates_divergent_case/0,
        fun three_candidates_condorcet_over_borda_case/0,
        fun three_candidates_condorcet_over_irv_case/0
    ].

three_candidates_one_voter_case() ->
    Ballot = ballot:make([a, b, c]),
    Winner = consensus:condorcet_winner([Ballot]),
    ?assertEqual(a, Winner),
    Rankings = consensus:condorcet_rankings([Ballot]),
    ?assertEqual([a, b, c], Rankings).

three_candidates_majority_case() ->
    Ballot1  = ballot:make([c, b, a]),
    Ballot2  = ballot:make([a, b, c]),
    Ballot3  = ballot:make([a, c, b]),
    Ballot4  = ballot:make([b, a, c]),
    Ballot5  = ballot:make([a, c, b]),
    Ballots  = [Ballot1, Ballot2, Ballot3, Ballot4, Ballot5],
    Winner   = consensus:condorcet_winner(Ballots),
    ?assertEqual(a, Winner),
    Rankings = consensus:condorcet_rankings(Ballots),
    ?assertEqual([a, c, b], Rankings).

three_candidates_divergent_case() ->
    Ballots = weighted([{499, abc_ballot()}, {3, bca_ballot()}, {498, cba_ballot()}]),
    ?assertEqual(b, consensus:condorcet_winner(Ballots)).

three_candidates_condorcet_over_borda_case() ->
    % https://en.wikipedia.org/wiki/Condorcet_criterion#Borda_count
    Ballots = weighted([{3, abc_ballot()}, {2, bca_ballot()}]),
    ?assertEqual(a, consensus:condorcet_winner(Ballots)).

three_candidates_condorcet_over_irv_case() ->
    % https://en.wikipedia.org/wiki/Condorcet_criterion#Instant-runoff_voting
    Ballots = weighted([{35, abc_ballot()}, {34, cba_ballot()}, {31, bca_ballot()}]),
    ?assertEqual(b, consensus:condorcet_winner(Ballots)).

%%% PRIVATE FUNCTIONS

abc_ballot() -> ballot:make([a, b, c]).

bca_ballot() -> ballot:make([b, c, a]).

cba_ballot() -> ballot:make([c, b, a]).

weighted(BallotCounts) ->
    lists:flatten([lists:duplicate(Count, Ballot) || {Count, Ballot} <- BallotCounts]).

