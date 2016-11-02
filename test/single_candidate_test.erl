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
    Winner = schulze_vote:winner([a]),
    ?assertEqual(a, Winner).

single_candidate_multiple_voter_case() ->
    Winner = schulze_vote:winner([a, a, a, a]),
    ?assertEqual(a, Winner).

%%% PRIVATE FUNCTIONS

