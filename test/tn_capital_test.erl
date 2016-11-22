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
                fun tn_capital_winner_case/0,
                fun tn_capital_rankings_case/0,
                fun tn_capital_rankings_mod_case/0
            ]
    }.

tn_capital_winner_case() ->
    Winner = consensus:schulze_winner(tn_ballots()),
    ?assertEqual(nashville, Winner).

tn_capital_rankings_case() ->
    Rankings = consensus:schulze_rankings(tn_ballots()),
    ?assertEqual([nashville, chattanooga, knoxville, memphis], Rankings).

tn_capital_rankings_mod_case() ->
    Rankings = consensus:schulze_rankings(tn_mod_ballots()),
    ?assertEqual([chattanooga, nashville, knoxville, memphis], Rankings).

%%% PRIVATE FUNCTIONS

tn_ballots() ->
    % https://en.wikipedia.org/wiki/Condorcet_method
    %   #Example:_Voting_on_the_location_of_Tennessee.27s_capital
    BallotM = schulze_ballot:make([memphis, nashville, chattanooga, knoxville]),
    BallotN = schulze_ballot:make([nashville, chattanooga, knoxville, memphis]),
    BallotC = schulze_ballot:make([chattanooga, knoxville, nashville, memphis]),
    BallotK = schulze_ballot:make([knoxville, chattanooga, nashville, memphis]),
    lists:flatten([
        lists:duplicate(42, BallotM),
        lists:duplicate(26, BallotN),
        lists:duplicate(15, BallotC),
        lists:duplicate(17, BallotK)
    ]).

tn_mod_ballots() ->
    % Memphis voters put Chat as 2nd choice.
    BallotM = schulze_ballot:make([memphis, chattanooga, nashville, knoxville]),
    BallotN = schulze_ballot:make([nashville, chattanooga, knoxville, memphis]),
    BallotC = schulze_ballot:make([chattanooga, knoxville, nashville, memphis]),
    BallotK = schulze_ballot:make([knoxville, chattanooga, nashville, memphis]),
    lists:flatten([
        lists:duplicate(42, BallotM),
        lists:duplicate(26, BallotN),
        lists:duplicate(15, BallotC),
        lists:duplicate(17, BallotK)
    ]).
