-module('gallagher_index_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

% Gallagher Index

gallagher_index_test_() ->
    % Cf. Lijphart, Arend, _Patterns of Democracy_, 1999. pg158.
    % measure of disproportionality:
    %   G = √(1/2 * Σ(Vi - Si)^2)
    %       where Vi is vote % at index i
    %       and Si is seat % at index i
    [
        fun gallagher_index_2023_new_zealand_case/0,
        fun gallagher_index_2024_european_parliament_case/0,
        fun gallagher_index_even_three_case/0,
        fun gallagher_index_even_two_case/0,
        fun gallagher_index_extreme_skew_case/0,
        fun gallagher_index_no_skew_case/0,
        fun gallagher_index_uk2015parl_case/0,
        fun gallagher_index_us2008potus_case/0,
        fun gallagher_index_us2008potus_ec_case/0,
        fun gallagher_index_us2016potus_case/0,
        fun gallagher_index_us2016potus_ec_case/0,
        fun gallagher_index_us2020potus_case/0,
        fun gallagher_index_us2020potus_ec_case/0
    ].

% In descending order of Gallagher Index / disproportionality / "skew".
gallagher_index_extreme_skew_case() ->
    GIdx = consensus:gallagher_index(extreme_skew()),
    ?assertEqual(GIdx, 0.9).

% https://en.wikipedia.org/wiki/2008_United_States_presidential_election
gallagher_index_us2008potus_case() ->
    GIdx = consensus:gallagher_index(us2008potus()),
    ?assert(GIdx > 0.4635),
    ?assert(GIdx < 0.4636).

% https://en.wikipedia.org/wiki/2016_United_States_presidential_election
gallagher_index_us2016potus_case() ->
    GIdx = consensus:gallagher_index(us2016potus()),
    ?assert(GIdx > 0.5058),
    ?assert(GIdx < 0.5059).

% https://en.wikipedia.org/wiki/2020_United_States_presidential_election
gallagher_index_us2020potus_case() ->
    GIdx = consensus:gallagher_index(us2020potus()),
    ?assert(GIdx > 0.4810),
    ?assert(GIdx < 0.4811).

% https://en.wikipedia.org/wiki/2015_United_Kingdom_general_election
gallagher_index_uk2015parl_case() ->
    GIdx = consensus:gallagher_index(uk2015parl()),
    ?assert(GIdx > 0.15),
    ?assert(GIdx < 0.16).

gallagher_index_us2008potus_ec_case() ->
    GIdx = consensus:gallagher_index(us2008potus_ec()),
    ?assert(GIdx > 0.142),
    ?assert(GIdx < 0.143).

gallagher_index_us2016potus_ec_case() ->
    GIdx = consensus:gallagher_index(us2016potus_ec()),
    ?assert(GIdx > 0.078),
    ?assert(GIdx < 0.079).

gallagher_index_us2020potus_ec_case() ->
    GIdx = consensus:gallagher_index(us2020potus_ec()),
    ?assert(GIdx > 0.050),
    ?assert(GIdx < 0.051).

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

% https://en.wikipedia.org/wiki/2024_European_Parliament_election
% Seat totals and popular-vote % per EP group. The article folds the
% Non-Inscrits voters into its "Others" vote row, so they are paired here.
gallagher_index_2024_european_parliament_case() ->
    GIdx = consensus:gallagher_index(ep2024_groups()),
    ?assert(GIdx > 0.062),
    ?assert(GIdx < 0.063).

% https://en.wikipedia.org/wiki/2023_New_Zealand_general_election
% All 17 registered parties that contested the party vote, whose counts sum
% exactly to the 2,851,211 valid party votes; the article's separate 34,277
% "Independent" row is an electorate-vote figure and is excluded. Seats are
% the 122 of the 54th Parliament (120 proportional seats plus two Te Pati
% Maori overhang seats). Vote shares below one percent must be supplied as
% fractions to consensus_party:make/3; consensus_party:make_percent/3 takes
% percentages instead.
% G = 2.63, matching the value tabulated at
% https://en.wikipedia.org/wiki/Gallagher_index
gallagher_index_2023_new_zealand_case() ->
    GIdx = consensus:gallagher_index(nz2023()),
    ?assert(GIdx > 0.0263),
    ?assert(GIdx < 0.0264).

%%% PRIVATE FUNCTIONS

nz2023() ->
    [ consensus_party:make(Name, Seats, VoteShare) ||
      {Name, VoteShare, Seats} <- index_fixtures:nz2023_party_results() ].

even(3) -> [
    consensus_party:make_percent(jack_johnson, 1, 33.33),
    consensus_party:make_percent(john_jackson, 1, 33.33),
    consensus_party:make_percent(jenn_jackson, 1, 33.33)
];
even(2) -> [
    consensus_party:make_percent(jack_johnson, 1, 50.00),
    consensus_party:make_percent(john_jackson, 1, 50.00)
].

extreme_skew() -> [
    consensus_party:make_percent(usurper, 1, 10.00),
    consensus_party:make_percent(cheated, 0, 90.00)
].

no_skew() -> [
    consensus_party:make_percent(popular, 1, 100.00),
    consensus_party:make_percent(pariah,  0,   0.00)
].

uk2015parl()  -> [
    % party_name, seat_share, vote_pc
    % 650 total seats in play
    consensus_party:make_percent(con, 330, 36.9),
    consensus_party:make_percent(lab, 232, 30.4),
    consensus_party:make_percent(snp,  56,  4.7),
    consensus_party:make_percent(lib,   8,  7.9),
    consensus_party:make_percent(ukip,  1, 12.3),
    consensus_party:make_percent(grn,   1,  3.8)
].

us2008potus() -> [
    % party_name, electoral_votes, vote_pc
    % 1 Presidential "seat" in play
    consensus_party:make_percent(obama,  1, 52.9),
    consensus_party:make_percent(mccain, 0, 45.6)
].

us2008potus_ec() -> [
    % party_name, electoral_votes, vote_pc
    % 538 Electoral College "seats" in play
    consensus_party:make_percent(dem, 365, 52.9),
    consensus_party:make_percent(gop, 173, 45.7)
].

us2016potus() -> [
    % party_name, electoral_votes, vote_pc
    % 1 Presidential "seat" in play
    consensus_party:make_percent(trump,   1, 46.72),
    consensus_party:make_percent(clinton, 0, 47.73)
].

us2020potus() -> [
    % party_name, electoral_votes, vote_pc
    % 1 Presidential "seat" in play
    consensus_party:make_percent(biden, 1, 51.0),
    consensus_party:make_percent(trump, 0, 47.2)
].

us2016potus_ec() -> [
    % party_name, electoral_votes, vote_pc
    % 538 Electoral College "seats" in play
    consensus_party:make_percent(gop, 306, 46.72),
    consensus_party:make_percent(dem, 232, 47.73)
    % counting "faithless" electors as if they were not
].

us2020potus_ec() -> [
    % party_name, electoral_votes, vote_pc
    % 538 Electoral College "seats" in play
    consensus_party:make_percent(dem, 306, 51.0),
    consensus_party:make_percent(gop, 232, 47.2)
    % counting "faithless" electors as if they were not
].

ep2024_groups() ->
    [ consensus_party:make_percent(Group, Seats, VotePercent) ||
      {Group, Seats, VotePercent} <- index_fixtures:ep2024_group_results() ].
