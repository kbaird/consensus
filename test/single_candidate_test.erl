-module('single_candidate_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

% SINGLE_CANDIDATE

single_candidate_setup()     -> ok.
single_candidate_teardown(_) -> ok.

single_candidate_test_() ->
    {setup, fun single_candidate_setup/0,
            fun single_candidate_teardown/1,
            [
                fun single_candidate_single_voter_case/0,
                fun single_candidate_multiple_voter_case/0
            ]
    }.

single_candidate_single_voter_case() ->
    Ballot = schulze_vote:make_ballot([a]),
    Winner = schulze_vote:winner([Ballot]),
    ?assertEqual(a, Winner).

single_candidate_multiple_voter_case() ->
    Ballot1 = schulze_vote:make_ballot([a]),
    Ballot2 = schulze_vote:make_ballot([a]),
    Ballot3 = schulze_vote:make_ballot([a]),
    Ballot4 = schulze_vote:make_ballot([a]),
    Ballots = [Ballot1, Ballot2, Ballot3, Ballot4],
    Winner  = schulze_vote:winner(Ballots),
    ?assertEqual(a, Winner).

%%% PRIVATE FUNCTIONS

