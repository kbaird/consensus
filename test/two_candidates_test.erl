-module('two_candidates_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").
-include_lib("elections.hrl").

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
    Ballot = condorcet_ballot:make([a, b]),
    Winner = consensus:condorcet_winner([Ballot]),
    ?assertEqual(a, Winner).

two_candidates_majority_case() ->
    Ballot1 = condorcet_ballot:make([b, a]),
    Ballot2 = condorcet_ballot:make([a, b]),
    Ballots = [Ballot1, Ballot2, Ballot2, Ballot2],
    Winner  = consensus:condorcet_winner(Ballots),
    ?assertEqual(a, Winner).

two_candidates_unanimous_case() ->
    Ballots = [condorcet_ballot:make([a]) || _ <- lists:seq(1, 4)],
    Winner  = consensus:condorcet_winner(Ballots),
    ?assertEqual(a, Winner).

%%% PRIVATE FUNCTIONS

