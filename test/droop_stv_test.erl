-module('droop_stv_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").
-include_lib("elections.hrl").

droop_stv_setup()     -> ok.
droop_stv_teardown(_) -> ok.

droop_stv_test_() ->
    {setup, fun droop_stv_setup/0,
            fun droop_stv_teardown/1,
            [
                fun droop_two_winners_three_candidates_stv_stv_case/0,
                %fun droop_five_winners_six_candidates_stv_stv_case/0,
                fun droop_three_winners_five_candidates_stv_stv_case/0
            ]
    }.

% https://en.wikipedia.org/wiki/Droop_quota#An_example_of_use_in_STV
droop_two_winners_three_candidates_stv_stv_case() ->
    BradOnlyBallots   = [ballot:make([brad])   || _ <- lists:seq(1, 30)],
    CarterOnlyBallots = [ballot:make([carter]) || _ <- lists:seq(1, 25)],
    MixedBallots      = [ballot:make([andrea, carter]) || _ <- lists:seq(1, 45)],
    Ballots = BradOnlyBallots ++ CarterOnlyBallots ++ MixedBallots,
    Winners = consensus:droop_winners(2, Ballots),
    ?assertEqual([{andrea, 45}, {carter, 36}], Winners).

% https://en.wikipedia.org/wiki/Comparison_of_the_Hare_and_Droop_quotas#Scenario_1
droop_five_winners_six_candidates_stv_stv_case() ->
    Ballot1s = [ballot:make([andrea, carter, brad]) || _ <- lists:seq(1, 31)],
    Ballot2s = [ballot:make([carter, andrea, brad]) || _ <- lists:seq(1, 30)],
    Ballot3s = [ballot:make([brad, andrea, carter]) || _ <- lists:seq(1, 2)],
    Ballot4s = [ballot:make([delilah, scott, jennifer]) || _ <- lists:seq(1, 20)],
    Ballot5s = [ballot:make([scott, delilah, jennifer]) || _ <- lists:seq(1, 20)],
    Ballot6s = [ballot:make([jennifer, delilah, scott]) || _ <- lists:seq(1, 17)],
    Ballots = Ballot1s ++ Ballot2s ++ Ballot3s ++ Ballot4s ++ Ballot5s ++ Ballot6s,
    [ Andrea, Carter, Brad, Delilah, Scott ] = consensus:droop_winners(5, Ballots),
    {andrea, _} = Andrea,
    {carter, _} = Carter,
    {brad, _} = Brad,
    {delilah, _} = Delilah,
    {scott, _} = Scott.

% https://en.wikipedia.org/wiki/Comparison_of_the_Hare_and_Droop_quotas#Scenario_2
droop_three_winners_five_candidates_stv_stv_case() ->
    Ballot1s = [ballot:make([alex, bobbie, chris]) || _ <- lists:seq(1, 50)],
    Ballot2s = [ballot:make([jo]) || _ <- lists:seq(1, 25)],
    Ballot3s = [ballot:make([kim]) || _ <- lists:seq(1, 24)],
    Ballots = Ballot1s ++ Ballot2s ++ Ballot3s,
    [ Alex | OtherWinners ] = consensus:droop_winners(3, Ballots),
    ?assertEqual({alex, 50}, Alex),
    ?assert(lists:member({bobbie, 25}, OtherWinners)),
    ?assert(lists:member({jo, 25}, OtherWinners)).

%%% PRIVATE FUNCTIONS

