-module('gallagher_index_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

% Gallagher Index

gallagher_index_setup()     -> ok.
gallagher_index_teardown(_) -> ok.

gallagher_index_test_() ->
    % Cf. Lijphart, Arend, _Patterns of Democracy_, 1999. pg158.
    % measure of disproportionality:
    %   G = √(1/2 * Σ(Vi - Si)^2)
    %       where Vi is vote % at index i
    %       and Si is seat % at index i
    {setup, fun gallagher_index_setup/0,
            fun gallagher_index_teardown/1,
            [
                fun gallagher_index_uk2015parl_case/0,
                fun gallagher_index_us2016potus_case/0,
                fun gallagher_index_us2016potus_ec_case/0,
                fun gallagher_index_even_three_case/0,
                fun gallagher_index_even_two_case/0,
                fun gallagher_index_extreme_skew_case/0,
                fun gallagher_index_no_skew_case/0
            ]
    }.

% In descending order of Gallagher Index / disproportionality / "skew".
gallagher_index_extreme_skew_case() ->
    GIdx = consensus:gallagher_index(extreme_skew()),
    ?assertEqual(GIdx, 0.9).

gallagher_index_us2016potus_case() ->
    GIdx = consensus:gallagher_index(us2016potus()),
    ?assert(GIdx > 0.5058),
    ?assert(GIdx < 0.5059).

gallagher_index_uk2015parl_case() ->
    GIdx = consensus:gallagher_index(uk2015parl()),
    ?assert(GIdx > 0.15),
    ?assert(GIdx < 0.16).

gallagher_index_us2016potus_ec_case() ->
    GIdx = consensus:gallagher_index(us2016potus_ec()),
    ?assert(GIdx > 0.078),
    ?assert(GIdx < 0.079).

gallagher_index_even_three_case() ->
    GIdx = consensus:gallagher_index(even(3)),
    % even this much is probably from base 10 math on a binary platform
    ?assert(GIdx < 0.000041).

gallagher_index_even_two_case() ->
    GIdx = consensus:gallagher_index(even(2)),
    ?assertEqual(GIdx, 0.0).

gallagher_index_no_skew_case() ->
    GIdx = consensus:gallagher_index(no_skew()),
    ?assertEqual(GIdx, 0.0).

%%% PRIVATE FUNCTIONS

even(3) -> [
    consensus_party:make(jack_johnson, 1, 33.33),
    consensus_party:make(john_jackson, 1, 33.33),
    consensus_party:make(jenn_jackson, 1, 33.33)
];
even(2) -> [
    consensus_party:make(jack_johnson, 1, 50.00),
    consensus_party:make(john_jackson, 1, 50.00)
].

extreme_skew() -> [
    consensus_party:make(usurper, 1, 10.00),
    consensus_party:make(cheated, 0, 90.00)
].

no_skew() -> [
    consensus_party:make(popular, 1, 100.00),
    consensus_party:make(pariah,  0,   0.00)
].

uk2015parl()  -> [
    % party_name, seat_share, vote_pc
    % 650 total seats in play
    consensus_party:make(con, 330, 36.9),
    consensus_party:make(lab, 232, 30.4),
    consensus_party:make(snp,  56,  4.7),
    consensus_party:make(lib,   8,  7.9),
    consensus_party:make(ukip,  1, 12.3),
    consensus_party:make(grn,   1,  3.8)
].

us2016potus() -> [
    % party_name, electoral_votes, vote_pc
    % 1 Presidential "seat" in play
    consensus_party:make(gop, 1, 46.72),
    consensus_party:make(dem, 0, 47.73)
].

us2016potus_ec() -> [
    % party_name, electoral_votes, vote_pc
    % 538 Electoral College "seats" in play
    consensus_party:make(gop, 306, 46.72),
    consensus_party:make(dem, 232, 47.73)
].
