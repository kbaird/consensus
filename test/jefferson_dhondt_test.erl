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
    Votes    = [ {a, 100000}, {b, 80000}, {c, 30000}, {d, 20000} ],
    Rankings = consensus:jefferson_dhondt_rankings(Votes, 8),
    ?assertEqual([{a, 4, 3.4}, {b, 3, 2.8}, {c, 1, 1.1}, {d, 0, 0.7}], Rankings).

%%% PRIVATE FUNCTIONS

