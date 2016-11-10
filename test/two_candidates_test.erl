-module('two_candidates_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").
-include_lib("schulze.hrl").

two_candidates_setup()     -> ok.
two_candidates_teardown(_) -> ok.

two_candidates_test_() ->
    {setup, fun two_candidates_setup/0,
            fun two_candidates_teardown/1,
            [
                fun two_candidates_one_voter_case/0,
                fun two_candidates_majority_case/0,
                fun two_candidates_unanimous_case/0
            ]
    }.

two_candidates_one_voter_case() ->
    Ballot = schulze_vote:make_ballot([a, b]),
    Winner = schulze_vote:winner([Ballot]),
    ?assertEqual(a, Winner).

two_candidates_majority_case() ->
    Ballot1 = schulze_vote:make_ballot([b, a]),
    Ballot2 = schulze_vote:make_ballot([a, b]),
    Ballot3 = schulze_vote:make_ballot([a, b]),
    Ballot4 = schulze_vote:make_ballot([a, b]),
    Ballots = [Ballot1, Ballot2, Ballot3, Ballot4],
    Winner  = schulze_vote:winner(Ballots),
    ?assertEqual(a, Winner).

two_candidates_unanimous_case() ->
    Ballot1 = schulze_vote:make_ballot([a, b]),
    Ballot2 = schulze_vote:make_ballot([a, b]),
    Ballot3 = schulze_vote:make_ballot([a, b]),
    Ballot4 = schulze_vote:make_ballot([a, b]),
    Ballots = [Ballot1, Ballot2, Ballot3, Ballot4],
    Winner  = schulze_vote:winner(Ballots),
    ?assertEqual(a, Winner).

%%% PRIVATE FUNCTIONS

