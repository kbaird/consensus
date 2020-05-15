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
                fun webster_sainte_lague_2016_saxony_anhalt_case/0
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
    Votes    = [ {spd, 679872}, {cdu, 1447249}, {afd, 809311}, {fdp, 445430}, {grn, 1622631}, {lnk, 156211}, {alfa, 54764}, {odp, 38509}, {npd, 23605}, {pp, 21773} ],
    Rankings = consensus:webster_sainte_lague_rankings(Votes, 143, 0.05),
    [{grn, 47, _}, {cdu, 42, _}, {afd, 23, _}, {spd, 19, _}, {fdp, 12, _}] = Rankings.

% https://en.wikipedia.org/wiki/2016_Rhineland-Palatinate_state_election
webster_sainte_lague_2016_rhineland_palatinate_case() ->
    Votes    = [ {spd, 771848}, {cdu, 677507}, {afd, 268628}, {fdp, 132294}, {grn, 113261}, {lnk, 59970} ],
    Rankings = consensus:webster_sainte_lague_rankings(Votes, 101, 0.05),
    [{spd, 39, _}, {cdu, 35, _}, {afd, 14, _}, {fdp, 7, _}, {grn, 6, _}] = Rankings.

% https://en.wikipedia.org/wiki/2016_Saxony-Anhalt_state_election
webster_sainte_lague_2016_saxony_anhalt_case() ->
    Votes    = [ {cdu, 334139}, {afd, 272496}, {lnk, 183290}, {spd, 119368}, {grn, 58209}, {fdp, 54565}, {fw, 24269}, {npd, 21230}, {heap, 16611}, {apa, 11653} ],
    Rankings = consensus:webster_sainte_lague_rankings(Votes, 87, 0.05),
    [ {cdu, 30, _}, {afd, 24, _}, {lnk, 16, _}, {spd, 11, _}, {grn, 5, _}] = Rankings.

