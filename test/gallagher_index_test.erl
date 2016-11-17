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

uk2015parl()  -> [  {con, 330, 36.1}, {lab, 232, 29.0},
                    {snp, 56,   1.7}, {lib,   8, 23.0} ].
us2016potus() -> [ {trump, 306, 46.72}, {clinton, 232, 47.73} ].
