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
    ?assert(GIdx > 35.15),
    ?assert(GIdx < 35.20).

gallagher_index_us2016potus_case() ->
    GIdx = consensus:gallagher_index(us2016potus()),
    ?assert(GIdx > 46.70),
    ?assert(GIdx < 46.75).

%%% PRIVATE FUNCTIONS

uk2015parl()  -> [
    consensus_party:make(con, 330, 36.9),
    consensus_party:make(lab, 232, 30.4),
    consensus_party:make(snp,  56,  4.7),
    consensus_party:make(lib,   8,  7.9),
    consensus_party:make(ukip,  1, 12.3),
    consensus_party:make(grn,   1,  3.8)
].
us2016potus() -> [
    consensus_party:make(gop, 306, 46.72),
    consensus_party:make(dem, 232, 47.73)
].
