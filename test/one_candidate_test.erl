-module('one_candidate_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

one_candidate_test_() ->
    [
        fun one_candidate_one_voter_case/0
    ].

one_candidate_one_voter_case() ->
    Ballot = ballot:make([a]),
    Winner = consensus:condorcet_winner([Ballot]),
    ?assertEqual(a, Winner).

