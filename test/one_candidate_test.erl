-module('one_candidate_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").
-include_lib("schulze.hrl").

one_candidate_setup()     -> ok.
one_candidate_teardown(_) -> ok.

one_candidate_test_() ->
    {setup, fun one_candidate_setup/0,
            fun one_candidate_teardown/1,
            [
                fun one_candidate_one_voter_case/0,
                fun one_candidate_multiple_voter_case/0
            ]
    }.

one_candidate_one_voter_case() ->
    Ballot = schulze_vote:make_ballot([a]),
    Winner = schulze_vote:winner([Ballot]),
    ?assertEqual(a, Winner).

one_candidate_multiple_voter_case() ->
    Ballot1 = schulze_vote:make_ballot([a]),
    Ballot2 = schulze_vote:make_ballot([a]),
    Ballot3 = schulze_vote:make_ballot([a]),
    Ballot4 = schulze_vote:make_ballot([a]),
    Ballots = [Ballot1, Ballot2, Ballot3, Ballot4],
    Winner  = schulze_vote:winner(Ballots),
    ?assertEqual(a, Winner).

%%% PRIVATE FUNCTIONS

