-module('schulze_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").
-include_lib("condorcet.hrl").

% Schulze
% Taken from http://wiki.electorama.com/wiki/Schulze_method

schulze_ex1_setup()     -> ok.
schulze_ex1_teardown(_) -> ok.

schulze_ex1_test_() ->
    {setup, fun schulze_ex1_setup/0,
            fun schulze_ex1_teardown/1,
            [
                fun schulze_ex1_winner_case/0,
                fun schulze_ex1_rankings_case/0
            ]
    }.

schulze_ex1_winner_case() ->
    Winner = consensus:condorcet_winner(schulze_ex1_ballots()),
    ?assertEqual(e, Winner).

schulze_ex1_rankings_case() ->
    Rankings = consensus:condorcet_rankings(schulze_ex1_ballots()),
    ?assertEqual([e, a, c, b, d], Rankings).

%%% PRIVATE FUNCTIONS

schulze_ex1_ballots() ->
    lists:flatten([
        lists:duplicate(5, ballot([a, c, b, e, d])),
        lists:duplicate(5, ballot([a, d, e, c, b])),
        lists:duplicate(8, ballot([b, e, d, a, c])),
        lists:duplicate(3, ballot([c, a, b, e, d])),
        lists:duplicate(7, ballot([c, a, e, b, d])),
        lists:duplicate(2, ballot([c, b, a, d, e])),
        lists:duplicate(7, ballot([d, c, e, b, a])),
        lists:duplicate(8, ballot([e, b, a, d, c]))
    ]).

ballot(L) -> condorcet_ballot:make(L).
