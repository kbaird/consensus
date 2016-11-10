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
    Ballot = schulze_vote:make_ballot([a, b, c]),
    Winner = schulze_vote:winner([Ballot]),
    ?assertEqual(a, Winner).

three_candidates_majority_case() ->
    Ballot1 = schulze_vote:make_ballot([b, c, a]),
    Ballot2 = schulze_vote:make_ballot([a, b, c]),
    Ballot3 = schulze_vote:make_ballot([a, b, c]),
    Ballot4 = schulze_vote:make_ballot([a, b, c]),
    Ballots = [Ballot1, Ballot2, Ballot3, Ballot4],
    Winner  = schulze_vote:winner(Ballots),
    ?assertEqual(a, Winner).

three_candidates_unanimous_case() ->
    Ballot1 = schulze_vote:make_ballot([a, b, c]),
    Ballot2 = schulze_vote:make_ballot([a, b, c]),
    Ballot3 = schulze_vote:make_ballot([a, b, c]),
    Ballot4 = schulze_vote:make_ballot([a, b, c]),
    Ballots = [Ballot1, Ballot2, Ballot3, Ballot4],
    Winner  = schulze_vote:winner(Ballots),
    ?assertEqual(a, Winner).

%%% PRIVATE FUNCTIONS

