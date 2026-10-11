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
                fun webster_sainte_lague_2021_federal_case/0,
                fun webster_sainte_lague_2023_new_zealand_case/0,
                fun webster_sainte_lague_2024_german_european_parliament_case/0
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
    % the real overhang allocation gave to CDU (real: cdu 42, fdp 12). This compensated
    % (levelling) allocation is out of scope for webster_sainte_lague_mmp_rankings/4,
    % which models only the overhang a party keeps for winning more electorates than
    % its entitlement.
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
    % not a constituency win, and is excluded here. Real CDU/CSU = 197. This state-level,
    % overhang-compensated allocation is out of scope for
    % webster_sainte_lague_mmp_rankings/4, which models only the overhang a party keeps
    % for winning more electorates than its entitlement.
    [ {spd, 208, _}, {cdu, 194, _}, {grn, 119, _},
      {fdp, 92, _}, {afd, 83, _}, {lnk, 39, _}] = Rankings.

% https://en.wikipedia.org/wiki/2024_European_Parliament_election_in_Germany
% Germany elects 96 MEPs by the Webster/Sainte-Lague method in a single
% nationwide constituency with no electoral threshold; the article reports
% all parties not listed below with 0 seats.
webster_sainte_lague_2024_german_european_parliament_case() ->
    Votes     = [ {cdu, 9431567}, {afd, 6324008}, {spd, 5548528},
                  {greens, 4736913}, {csu, 2513300}, {bsw, 2453652},
                  {fdp, 2060457}, {linke, 1091268}, {free_voters, 1062132},
                  {volt, 1023161}, {partei, 775392}, {animal_protection, 570498},
                  {oedp, 257968}, {family, 243975}, {progress, 227631} ],
    Rankings  = consensus:webster_sainte_lague_rankings(Votes, 96),
    SeatsByName = maps:from_list([{Party, Seats} || {Party, Seats, _} <- Rankings]),
    ?assertEqual(96, lists:sum(maps:values(SeatsByName))),
    ?assertEqual(23, maps:get(cdu,       SeatsByName)),
    ?assertEqual(15, maps:get(afd,       SeatsByName)),
    ?assertEqual(14, maps:get(spd,       SeatsByName)),
    ?assertEqual(12, maps:get(greens,    SeatsByName)),
    ?assertEqual(6,  maps:get(csu,       SeatsByName)),
    ?assertEqual(6,  maps:get(bsw,       SeatsByName)),
    ?assertEqual(5,  maps:get(fdp,       SeatsByName)),
    ?assertEqual(3,  maps:get(linke,     SeatsByName)),
    ?assertEqual(3,  maps:get(free_voters, SeatsByName)),
    ?assertEqual(3,  maps:get(volt,      SeatsByName)),
    ?assertEqual(2,  maps:get(partei,    SeatsByName)),
    ?assertEqual(1,  maps:get(animal_protection, SeatsByName)),
    ?assertEqual(1,  maps:get(oedp,      SeatsByName)),
    ?assertEqual(1,  maps:get(family,    SeatsByName)),
    ?assertEqual(1,  maps:get(progress,  SeatsByName)).

% https://en.wikipedia.org/wiki/2023_New_Zealand_general_election
% New Zealand fills 120 seats proportionally by Sainte-Lague (48 party-list
% seats plus 72 electorate seats); a party qualifies by winning 5% of the
% party vote or an electorate seat, so Te Pati Maori (3.08% but six
% electorates) is included and TOP and the other sub-threshold parties are
% not. A pure Sainte-Lague apportionment of the 120 seats reproduces the
% official totals of the five regular parties exactly; Te Pati Maori is
% apportioned 4 and keeps 6, the two overhang seats that grow the 54th
% Parliament to 122. The overhang model reproduces the official electorate
% and list seats: National 43/5, Labour 17/17, Green 3/12, ACT 2/9, NZ First
% 0/8, Te Pati Maori 6/0.
webster_sainte_lague_2023_new_zealand_case() ->
    Votes        = [ {national, 1085851}, {labour, 767540}, {green, 330907},
                     {act, 246473}, {nz_first, 173553}, {te_pati_maori, 87844} ],
    Rankings     = consensus:webster_sainte_lague_rankings(Votes, 120),
    Rankings     = consensus:webster_sainte_lague_rankings(Votes, 120, 0.0),
    SeatsByName   = maps:from_list([{Party, Seats} || {Party, Seats, _} <- Rankings]),
    ?assertEqual(120, lists:sum(maps:values(SeatsByName))),
    ?assertEqual(48, maps:get(national,      SeatsByName)),
    ?assertEqual(34, maps:get(labour,        SeatsByName)),
    ?assertEqual(15, maps:get(green,         SeatsByName)),
    ?assertEqual(11, maps:get(act,           SeatsByName)),
    ?assertEqual(8,  maps:get(nz_first,      SeatsByName)),
    ?assertEqual(4,  maps:get(te_pati_maori, SeatsByName)),

    ElectorateSeats = #{ national => 43, labour => 17, green => 3,
                         act => 2, nz_first => 0, te_pati_maori => 6 },
    {MmpRankings, ChamberSize} =
        consensus:webster_sainte_lague_mmp_rankings(Votes, 120, 0.0, ElectorateSeats),
    TotalAndListByName = maps:from_list([{Party, {Total, List}} ||
                                         {Party, Total, List} <- MmpRankings]),
    ?assertEqual(122, ChamberSize),
    ?assertEqual({48, 5},  maps:get(national,       TotalAndListByName)),
    ?assertEqual({34, 17}, maps:get(labour,         TotalAndListByName)),
    ?assertEqual({15, 12}, maps:get(green,          TotalAndListByName)),
    ?assertEqual({11, 9},  maps:get(act,            TotalAndListByName)),
    ?assertEqual({8,  8},  maps:get(nz_first,       TotalAndListByName)),
    ?assertEqual({6,  0},  maps:get(te_pati_maori,  TotalAndListByName)).

