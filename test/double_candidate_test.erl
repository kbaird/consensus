-module('double_candidate_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

double_candidate_setup()     -> ok.
double_candidate_teardown(_) -> ok.

double_candidate_test_() ->
    {setup, fun double_candidate_setup/0,
            fun double_candidate_teardown/1,
            [
                fun double_candidate_single_voter_case/0,
                fun double_candidate_majority_case/0,
                fun double_candidate_unanimous_case/0
            ]
    }.

double_candidate_single_voter_case() ->
    Ballot = schulze_vote:make_ballot([a, b]),
    Winner = schulze_vote:winner([Ballot]),
    ?assertEqual(a, Winner).

double_candidate_majority_case() ->
    Ballot1 = schulze_vote:make_ballot([a, b]),
    Ballot2 = schulze_vote:make_ballot([a, b]),
    Ballot3 = schulze_vote:make_ballot([a, b]),
    Ballot4 = schulze_vote:make_ballot([b, a]),
    Ballots = [Ballot1, Ballot2, Ballot3, Ballot4],
    Winner  = schulze_vote:winner(Ballots),
    ?assertEqual(a, Winner).

double_candidate_unanimous_case() ->
    Ballot1 = schulze_vote:make_ballot([a, b]),
    Ballot2 = schulze_vote:make_ballot([a, b]),
    Ballot3 = schulze_vote:make_ballot([a, b]),
    Ballot4 = schulze_vote:make_ballot([a, b]),
    Ballots = [Ballot1, Ballot2, Ballot3, Ballot4],
    Winner  = schulze_vote:winner(Ballots),
    ?assertEqual(a, Winner).

%%% PRIVATE FUNCTIONS

