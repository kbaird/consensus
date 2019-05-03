-module('one_candidate_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

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
    Ballot = ballot:make([a]),
    Winner = consensus:condorcet_winner([Ballot]),
    ?assertEqual(a, Winner).

one_candidate_multiple_voter_case() ->
    Ballots = [ballot:make([a]) || _ <- lists:seq(1, 4)],
    Winner  = consensus:condorcet_winner(Ballots),
    ?assertEqual(a, Winner).

%%% PRIVATE FUNCTIONS

