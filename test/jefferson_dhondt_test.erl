-module('jefferson_dhondt_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").
-include_lib("elections.hrl").

jefferson_dhondt_setup()     -> ok.
jefferson_dhondt_teardown(_) -> ok.

% https://en.wikipedia.org/wiki/D%27Hondt_method#Example
jefferson_dhondt_test_() ->
    {setup, fun jefferson_dhondt_setup/0,
            fun jefferson_dhondt_teardown/1,
            [
                fun jefferson_dhondt_basic_case/0
            ]
    }.

jefferson_dhondt_basic_case() ->
    % Dividing vote totals by 10K
    BallotsA = [ballot:make([], a) || _ <- lists:seq(1, 10)],
    BallotsB = [ballot:make([], b) || _ <- lists:seq(1, 8)],
    BallotsC = [ballot:make([], c) || _ <- lists:seq(1, 3)],
    BallotsD = [ballot:make([], d) || _ <- lists:seq(1, 2)],
    Ballots  = lists:flatten([BallotsA, BallotsB, BallotsC, BallotsD]),
    Rankings = consensus:jefferson_dhondt_rankings(Ballots, 8),
    ?assertEqual([{a, 4, 3.4}, {b, 3, 2.8}, {c, 1, 1.1}, {d, 0, 0.7}], Rankings).

%%% PRIVATE FUNCTIONS

