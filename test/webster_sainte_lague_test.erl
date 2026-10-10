-module('webster_sainte_lague_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

webster_sainte_lague_setup()     -> ok.
webster_sainte_lague_teardown(_) -> ok.

% https://en.wikipedia.org/wiki/Webster/Sainte-Lagu%C3%AB_method#Description_of_the_method
webster_sainte_lague_test_() ->
    {setup, fun webster_sainte_lague_setup/0,
            fun webster_sainte_lague_teardown/1,
            [
                fun webster_sainte_lague_basic_case/0,
                fun webster_sainte_lague_2016_baden_wuerttemberg_case/0,
                fun webster_sainte_lague_2016_rhineland_palatinate_case/0,
                fun webster_sainte_lague_2016_saxony_anhalt_case/0,
                fun webster_sainte_lague_2021_federal_case/0
            ]
    }.

% Not checking percentages, because I think they removed smaller totals from the reported results

webster_sainte_lague_basic_case() ->
    Votes    = [ {a, 53000}, {b, 24000}, {c, 23000} ],
    Rankings = consensus:webster_sainte_lague_rankings(Votes, 7),
    Rankings = consensus:webster_sainte_lague_rankings(Votes, 7, 0.0),
    ?assertEqual([{a, 3, 3.71}, {b, 2, 1.68}, {c, 2, 1.61}], Rankings).

% https://en.wikipedia.org/wiki/2016_Baden-W%C3%BCrttemberg_state_election
webster_sainte_lague_2016_baden_wuerttemberg_case() ->
    Votes    = [ {grn, 1622631}, {cdu, 1447249}, {afd, 809311},
                 {spd, 679872}, {fdp, 445430}, {lnk, 156211},
                 {alfa, 54764}, {odp, 38509}, {npd, 23605}, {pp, 21773} ],
    Rankings = consensus:webster_sainte_lague_rankings(Votes, 143, 0.05),
    % The real Landtag had 143 seats, but 120 are apportioned by Sainte-Lague and the
    % total grows to 143 via overhang (Ueberhang-) and leveling (Ausgleichs-) seats.
    % A pure Sainte-Lague apportionment of 143 seats therefore gives FDP the seat that
    % the real overhang allocation gave to CDU (real: cdu 42, fdp 12).
    [{grn, 47, _}, {cdu, 41, _}, {afd, 23, _}, {spd, 19, _}, {fdp, 13, _}] = Rankings.

% https://en.wikipedia.org/wiki/2016_Rhineland-Palatinate_state_election
webster_sainte_lague_2016_rhineland_palatinate_case() ->
    Votes    = [ {spd, 771848}, {cdu, 677507}, {afd, 268628},
                 {fdp, 132294}, {grn, 113261}, {lnk, 59970} ],
    Rankings = consensus:webster_sainte_lague_rankings(Votes, 101, 0.05),
    [{spd, 39, _}, {cdu, 35, _}, {afd, 14, _}, {fdp, 7, _}, {grn, 6, _}] = Rankings.

% https://en.wikipedia.org/wiki/2016_Saxony-Anhalt_state_election
webster_sainte_lague_2016_saxony_anhalt_case() ->
    Votes    = [ {cdu, 334139}, {afd, 272496}, {lnk, 183290},
                 {spd, 119368}, {grn, 58209}, {fdp, 54565},
                 {fw, 24269}, {npd, 21230}, {heap, 16611}, {apa, 11653} ],
    Rankings = consensus:webster_sainte_lague_rankings(Votes, 87, 0.05),
    [ {cdu, 30, _}, {afd, 25, _}, {lnk, 16, _}, {spd, 11, _}, {grn, 5, _}] = Rankings.

% https://en.wikipedia.org/wiki/2021_German_federal_election
webster_sainte_lague_2021_federal_case() ->
    Votes    = [ {spd, 11949756}, {cdu, 11173806}, {grn, 6848215},
                 {fdp, 5316698}, {afd, 4802097}, {lnk, 2269993} ],
    Rankings = consensus:webster_sainte_lague_rankings(Votes, 735, 0.05),
    % The real Bundestag had 736 seats (735 after the 2024 Berlin repeat). Working purely
    % from the parties above the threshold, a nationwide Sainte-Lague apportionment of 735
    % seats gives one or two seats to SPD/Greens that the real state-level, overhang-
    % compensated allocation gave elsewhere. The SSW's single seat (Stefan Seidler) is a
    % party-list seat won under the recognised-minority exemption from the 5% threshold,
    % not a constituency win, and is excluded here. Real CDU/CSU = 197.
    [ {spd, 208, _}, {cdu, 194, _}, {grn, 119, _},
      {fdp, 92, _}, {afd, 83, _}, {lnk, 39, _}] = Rankings.

