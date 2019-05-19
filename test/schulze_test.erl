-module('schulze_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

% Schulze
% Taken from http://wiki.electorama.com/wiki/Schulze_method

schulze_setup()     -> ok.
schulze_teardown(_) -> ok.

schulze_test_() ->
    {setup, fun schulze_setup/0,
            fun schulze_teardown/1,
            [
                fun schulze_ex1_winner_case/0,
                fun schulze_ex1_rankings_case/0,
                fun schulze_ex2_winner_case/0,
                fun schulze_ex2_rankings_case/0
            ]
    }.

schulze_ex1_winner_case() ->
    Winner = consensus:condorcet_winner(schulze_ex1_ballots()),
    ?assertEqual(e, Winner).

schulze_ex1_rankings_case() ->
    Rankings = consensus:condorcet_rankings(schulze_ex1_ballots()),
    ?assertEqual([e, a, c, b, d], Rankings).

schulze_ex2_winner_case() ->
    Winner = consensus:condorcet_winner(schulze_ex2_ballots()),
    ?assertEqual(d, Winner).

schulze_ex2_rankings_case() ->
    Rankings = consensus:condorcet_rankings(schulze_ex2_ballots()),
    ?assertEqual([d, a, c, b], Rankings).

%%% PRIVATE FUNCTIONS

% https://wiki.electorama.com/wiki/Schulze_method#Example_1
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

% https://wiki.electorama.com/wiki/Schulze_method#Example_2
schulze_ex2_ballots() ->
    lists:flatten([
        lists:duplicate(5, ballot([a, c, b, d])),
        lists:duplicate(2, ballot([a, c, d, b])),
        lists:duplicate(3, ballot([a, d, c, b])),
        lists:duplicate(4, ballot([b, a, c, d])),
        lists:duplicate(3, ballot([c, b, d, a])),
        lists:duplicate(3, ballot([c, d, b, a])),
        lists:duplicate(1, ballot([d, a, c, b])),
        lists:duplicate(5, ballot([d, b, a, c])),
        lists:duplicate(4, ballot([d, c, b, a]))
    ]).

ballot(L) -> ballot:make(L).
