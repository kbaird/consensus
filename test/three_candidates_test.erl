-module('three_candidates_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").
-include_lib("condorcet.hrl").

three_candidates_setup()     -> ok.
three_candidates_teardown(_) -> ok.

three_candidates_test_() ->
    {setup, fun three_candidates_setup/0,
            fun three_candidates_teardown/1,
            [
                fun three_candidates_one_voter_case/0,
                fun three_candidates_majority_case/0,
                fun three_candidates_unanimous_case/0,
                fun three_candidates_divergent_case/0,
                fun three_candidates_condorcet_over_borda_case/0,
                fun three_candidates_condorcet_over_irv_case/0
            ]
    }.

three_candidates_one_voter_case() ->
    Ballot = condorcet_ballot:make([a, b, c]),
    Winner = consensus:condorcet_winner([Ballot]),
    ?assertEqual(a, Winner).

three_candidates_majority_case() ->
    Ballot1  = condorcet_ballot:make([c, b, a]),
    Ballot2  = condorcet_ballot:make([a, b, c]),
    Ballot3  = condorcet_ballot:make([a, c, b]),
    Ballot4  = condorcet_ballot:make([b, a, c]),
    Ballot5  = condorcet_ballot:make([a, c, b]),
    Ballots  = [Ballot1, Ballot2, Ballot3, Ballot4, Ballot5],
    Winner   = consensus:condorcet_winner(Ballots),
    ?assertEqual(a, Winner),
    Rankings = consensus:condorcet_rankings(Ballots),
    ?assertEqual([a, c, b], Rankings).

three_candidates_unanimous_case() ->
    Ballot1 = condorcet_ballot:make([a, b, c]),
    Ballots = lists:duplicate(4, Ballot1),
    Winner  = consensus:condorcet_winner(Ballots),
    ?assertEqual(a, Winner).

three_candidates_divergent_case() ->
    Ballot1 = condorcet_ballot:make([a, b, c]),
    Ballot2 = condorcet_ballot:make([b, c, a]),
    Ballot3 = condorcet_ballot:make([c, b, a]),
    Ballots = lists:flatten([
        lists:duplicate(499, Ballot1),
        lists:duplicate(3,   Ballot2),
        lists:duplicate(498, Ballot3)
    ]),
    Winner  = consensus:condorcet_winner(Ballots),
    ?assertEqual(b, Winner).

three_candidates_condorcet_over_borda_case() ->
    Ballot1 = condorcet_ballot:make([a, b, c]),
    Ballot2 = condorcet_ballot:make([b, c, a]),
    Ballots = lists:flatten([
        lists:duplicate(3, Ballot1),
        lists:duplicate(2, Ballot2)
    ]),
    Winner  = consensus:condorcet_winner(Ballots),
    ?assertEqual(a, Winner).

three_candidates_condorcet_over_irv_case() ->
    Ballot1 = condorcet_ballot:make([a, b, c]),
    Ballot2 = condorcet_ballot:make([c, b, a]),
    Ballot3 = condorcet_ballot:make([b, c, a]),
    Ballots = lists:flatten([
        lists:duplicate(35, Ballot1),
        lists:duplicate(34, Ballot2),
        lists:duplicate(31, Ballot3)
    ]),
    Winner  = consensus:condorcet_winner(Ballots),
    ?assertEqual(b, Winner).

%%% PRIVATE FUNCTIONS

