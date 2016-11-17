-module('gallagher_index_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

% Gallagher Index

gallagher_index_setup()     -> ok.
gallagher_index_teardown(_) -> ok.

gallagher_index_test_() ->
    % Cf. Lijphart, Arend, _Patterns of Democracy_, 1999. pg158.
    {setup, fun gallagher_index_setup/0,
            fun gallagher_index_teardown/1,
            [
                fun gallagher_index_uk2015parl_case/0,
                fun gallagher_index_us2016potus_case/0
            ]
    }.

gallagher_index_uk2015parl_case() ->
    GIdx = consensus:gallagher_index(uk2015parl()),
    ?assertEqual(foo, GIdx).

gallagher_index_us2016potus_case() ->
    GIdx = consensus:gallagher_index(us2016potus()),
    ?assertEqual(foo, GIdx).

%%% PRIVATE FUNCTIONS

uk2015parl()  -> [  {con, 330, 36.9}, {lab, 232, 30.4},
                    {snp, 56,   4.7}, {lib,   8,  7.9} ].
us2016potus() -> [ {gop, 306, 46.72}, {dem, 232, 47.73} ].
