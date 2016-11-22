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
    Rankings = consensus:schulze_rankings(tn_ballots(mod)),
    ?assertEqual([chattanooga, nashville, knoxville, memphis], Rankings).

%%% PRIVATE FUNCTIONS

tn_ballots() -> tn_ballots(m).

tn_ballots(Atom) ->
    % https://en.wikipedia.org/wiki/Condorcet_method
    %   #Example:_Voting_on_the_location_of_Tennessee.27s_capital
    % Atom = mod means Memphis voters put Chat as 2nd choice.
    lists:flatten([
        lists:duplicate(42, ballot(Atom)),
        lists:duplicate(26, ballot(n)),
        lists:duplicate(15, ballot(c)),
        lists:duplicate(17, ballot(k))
    ]).

ballot(m)   -> schulze_ballot:make([memphis, nashville, chattanooga, knoxville]);
ballot(mod) -> schulze_ballot:make([memphis, chattanooga, nashville, knoxville]);
ballot(n)   -> schulze_ballot:make([nashville, chattanooga, knoxville, memphis]);
ballot(c)   -> schulze_ballot:make([chattanooga, knoxville, nashville, memphis]);
ballot(k)   -> schulze_ballot:make([knoxville, chattanooga, nashville, memphis]).

