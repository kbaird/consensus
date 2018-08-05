-module('borda_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").
-include_lib("elections.hrl").

borda_setup()     -> ok.
borda_teardown(_) -> ok.

% https://en.wikipedia.org/wiki/Borda_count#Example
borda_test_() ->
    {setup, fun borda_setup/0,
            fun borda_teardown/1,
            [
                fun borda_base0_case/0,
                fun borda_base1_case/0,
                fun borda_naura_case/0
            ]
    }.

borda_base0_case() ->
    Rankings = consensus:borda_rankings(base0, ballots()),
    ?assertEqual([
                  {andrew, 153},
                  {brian, 151},
                  {catherine, 205},
                  {david, 91}
                 ], Rankings).

borda_base1_case() ->
    Rankings = consensus:borda_rankings(base1, ballots()),
    ?assertEqual([
                  {andrew, 253},
                  {brian, 251},
                  {catherine, 305},
                  {david, 191}
                 ], Rankings).

borda_naura_case() ->
    Rankings = consensus:borda_rankings(naura, ballots()),
    ?assertEqual([
                  {andrew, 63.25},
                  {brian, 49.5},
                  {catherine, 52.5},
                  {david, 43.08333333333333}
                 ], Rankings).

%%% PRIVATE FUNCTIONS

ballots() ->
    Ballot1  = ballot:make([andrew, catherine, brian, david]),
    Ballot2  = ballot:make([catherine, brian, david, andrew]),
    Ballot3  = ballot:make([brian, catherine, david, andrew]),
    Ballot4  = ballot:make([david, catherine, brian, andrew]),
    lists:flatten([
        lists:duplicate(51, Ballot1),
        lists:duplicate(5, Ballot2),
        lists:duplicate(23, Ballot3),
        lists:duplicate(21, Ballot4)
    ]).
