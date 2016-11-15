-module('three_candidates_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").
-include_lib("schulze.hrl").

three_candidates_setup()     -> ok.
three_candidates_teardown(_) -> ok.

three_candidates_test_() ->
    {setup, fun three_candidates_setup/0,
            fun three_candidates_teardown/1,
            [
                fun three_candidates_one_voter_case/0,
                fun three_candidates_majority_case/0,
                fun three_candidates_unanimous_case/0
            ]
    }.

three_candidates_one_voter_case() ->
    Ballot = schulze_ballot:make([a, b, c]),
    Winner = consensus:schulze_winner([Ballot]),
    ?assertEqual(a, Winner).

three_candidates_majority_case() ->
    Ballot1  = schulze_ballot:make([c, b, a]),
    Ballot2  = schulze_ballot:make([a, b, c]),
    Ballot3  = schulze_ballot:make([a, c, b]),
    Ballot4  = schulze_ballot:make([b, a, c]),
    Ballot5  = schulze_ballot:make([a, c, b]),
    Ballots  = [Ballot1, Ballot2, Ballot3, Ballot4, Ballot5],
    Winner   = consensus:schulze_winner(Ballots),
    ?assertEqual(a, Winner),
    Rankings = consensus:schulze_rankings(Ballots),
    ?assertEqual([a, c, b], Rankings).

three_candidates_unanimous_case() ->
    Ballot1 = schulze_ballot:make([a, b, c]),
    Ballot2 = schulze_ballot:make([a, b, c]),
    Ballot3 = schulze_ballot:make([a, b, c]),
    Ballot4 = schulze_ballot:make([a, b, c]),
    Ballots = [Ballot1, Ballot2, Ballot3, Ballot4],
    Winner  = consensus:schulze_winner(Ballots),
    ?assertEqual(a, Winner).

%%% PRIVATE FUNCTIONS

