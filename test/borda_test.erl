-module('borda_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

borda_setup()     -> ok.
borda_teardown(_) -> ok.

% https://en.wikipedia.org/wiki/Borda_count#Example
borda_test_() ->
    {setup, fun borda_setup/0,
            fun borda_teardown/1,
            [
                fun borda_base0_case/0,
                fun borda_base1_case/0,
                fun borda_dowdell_case/0,
                fun borda_nauru_case/0,
                fun borda_nauru_us2016_case/0
            ]
    }.

borda_base0_case() ->
    Rankings = consensus:borda_rankings(base0, ballots()),
    ?assertEqual([
                  {catherine, 205},
                  {andrew, 153},
                  {brian, 151},
                  {david, 91}
                 ], Rankings).

borda_base1_case() ->
    Rankings = consensus:borda_rankings(base1, ballots()),
    ?assertEqual([
                  {catherine, 305},
                  {andrew, 253},
                  {brian, 251},
                  {david, 191}
                 ], Rankings).

borda_dowdell_case() ->
    Rankings = consensus:borda_rankings(dowdell, ballots()),
    ?assertEqual([
                  {andrew, 63.25},
                  {catherine, 52.5},
                  {brian, 49.5},
                  {david, 43.08333333333333}
                 ], Rankings).

borda_nauru_case() ->
    Rankings = consensus:borda_rankings(nauru, ballots()),
    ?assertEqual([
                  {andrew, 63.25},
                  {catherine, 52.5},
                  {brian, 49.5},
                  {david, 43.08333333333333}
                 ], Rankings).

borda_nauru_us2016_case() ->
    Rankings = consensus:borda_rankings(nauru, ballots(us2016)),
    [ {clinton, 62.0}, {trump, 60.5}, {johnson, GJ}, {stein, JS} ] = Rankings,
    ?assert(GJ > 52.16),
    ?assert(GJ < 52.17),
    ?assert(JS > 33.66),
    ?assert(JS < 33.67).

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

% These ballots are obviously not real.
% I wanted to play with one plausible scenario.
ballots(us2016) ->
    DemG = ballot:make([clinton, stein, johnson, trump]),
    DemL = ballot:make([clinton, johnson, stein, trump]),
    DemR = ballot:make([clinton, trump, johnson, stein]),
    GOPD = ballot:make([trump, clinton, johnson, stein]),
    GOPG = ballot:make([trump, stein, johnson, clinton]),
    GOPL = ballot:make([trump, johnson, clinton, stein]),
    GrnD = ballot:make([stein, clinton, johnson, trump]),
    GrnL = ballot:make([stein, johnson, clinton, trump]),
    LibD = ballot:make([johnson, clinton, stein, trump]),
    LibG = ballot:make([johnson, stein, clinton, trump]),
    LibR = ballot:make([johnson, trump, clinton, stein]),
    lists:flatten([
        lists:duplicate(30, GOPL),
        lists:duplicate(10, GOPD),
        lists:duplicate(2, GOPG),

        lists:duplicate(25, DemL),
        lists:duplicate(9, DemR),
        lists:duplicate(5, DemG),

        lists:duplicate(7, LibR),
        lists:duplicate(4, LibD),
        lists:duplicate(3, LibG),

        lists:duplicate(3, GrnD),
        lists:duplicate(2, GrnL)
    ]).
