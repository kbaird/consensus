-module('tn_capital_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").
-include_lib("schulze.hrl").

% tn_capital

tn_capital_setup()     -> ok.
tn_capital_teardown(_) -> ok.

tn_capital_test_() ->
    {setup, fun tn_capital_setup/0,
            fun tn_capital_teardown/1,
            [
                fun tn_capital_case/0
            ]
    }.

tn_capital_case() ->
    % https://en.wikipedia.org/wiki/Condorcet_method
    %   #Example:_Voting_on_the_location_of_Tennessee.27s_capital
    BallotM = schulze_vote:make_ballot([memphis, nashville, chattanooga, knoxville]),
    BallotN = schulze_vote:make_ballot([nashville, chattanooga, knoxville, memphis]),
    BallotC = schulze_vote:make_ballot([chattanooga, knoxville, nashville, memphis]),
    BallotK = schulze_vote:make_ballot([knoxville, chattanooga, nashville, memphis]),
    Ballots = lists:flatten([
        lists:duplicate(42, BallotM),
        lists:duplicate(26, BallotN),
        lists:duplicate(15, BallotC),
        lists:duplicate(17, BallotK)
    ]),
    Winner = schulze_vote:winner(Ballots),
    ?assertEqual(nashville, Winner#candidate.name).

%%% PRIVATE FUNCTIONS

