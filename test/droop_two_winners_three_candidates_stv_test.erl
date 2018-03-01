-module('droop_two_winners_three_candidates_stv_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").
-include_lib("condorcet.hrl").

droop_two_winners_three_candidates_stv_setup()     -> ok.
droop_two_winners_three_candidates_stv_teardown(_) -> ok.

droop_two_winners_three_candidates_stv_test_() ->
    {setup, fun droop_two_winners_three_candidates_stv_setup/0,
            fun droop_two_winners_three_candidates_stv_teardown/1,
            [
                fun droop_two_winners_three_candidates_stv_stv_case/0
            ]
    }.

% https://en.wikipedia.org/wiki/Droop_quota#An_example_of_use_in_STV
droop_two_winners_three_candidates_stv_stv_case() ->
    BradOnlyBallots   = [condorcet_ballot:make([brad])   || _ <- lists:seq(1, 30)],
    CarterOnlyBallots = [condorcet_ballot:make([carter]) || _ <- lists:seq(1, 25)],
    MixedBallots      = [condorcet_ballot:make([andrea, carter]) || _ <- lists:seq(1, 45)],
    Ballots = BradOnlyBallots ++ CarterOnlyBallots ++ MixedBallots,
    Winner  = consensus:droop_winners(Ballots),
    ?assertEqual([andrea, carter], Winner).

%%% PRIVATE FUNCTIONS

