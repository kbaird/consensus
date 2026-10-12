-module('jefferson_dhondt_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

% https://en.wikipedia.org/wiki/D%27Hondt_method#Example
jefferson_dhondt_test_() ->
    [
        fun jefferson_dhondt_basic_case/0,
        fun jefferson_dhondt_2024_french_european_parliament_case/0,
        fun jefferson_dhondt_2024_spanish_european_parliament_case/0,
        fun jefferson_dhondt_2022_danish_general_election_case/0,
        fun jefferson_dhondt_2022_faroe_islands_case/0,
        fun jefferson_dhondt_2022_greenland_case/0,
        % TODO: https://en.wikipedia.org/wiki/2024_San_Marino_general_election
        fun jefferson_dhondt_2026_danish_general_election_case/0,
        fun jefferson_dhondt_2026_faroe_islands_case/0,
        fun jefferson_dhondt_2026_greenland_case/0
    ].

jefferson_dhondt_basic_case() ->
    Votes    = [ {a, 100000}, {b, 80000}, {c, 30000}, {d, 20000} ],
    Rankings = consensus:jefferson_dhondt_rankings(Votes, 8),
    rankings_test_helpers:assert_default_threshold_matches(
        Votes, 8, fun consensus:jefferson_dhondt_rankings/2,
        fun consensus:jefferson_dhondt_rankings/3),
    ?assertEqual([{a, 4, 3.47}, {b, 3, 2.78}, {c, 1, 1.04}, {d, 0, 0.69}], Rankings).

% https://en.wikipedia.org/wiki/2024_European_Parliament_election_in_France
% France elects 81 MEPs by closed-list d'Hondt in a single national
% constituency with a 5% threshold of valid votes; seven lists pass the
% threshold and the article reports all other lists with 0 seats.
jefferson_dhondt_2024_french_european_parliament_case() ->
    Votes     = [ {rn, 7765936}, {ensemble, 3614646}, {socialist, 3424216},
                  {lfi, 2448703}, {republicans, 1794171}, {ecologists, 1361883},
                  {reconquete, 1353127}, {communists, 584067},
                  {rural_alliance, 582901} ],
    Rankings  = consensus:jefferson_dhondt_rankings(Votes, 81, 0.05),
    rankings_test_helpers:assert_seats(
        Rankings, 81,
        #{rn => 30, ensemble => 13, socialist => 13, lfi => 9, republicans => 6,
          ecologists => 5, reconquete => 5}),
    ?assertEqual(7, map_size(rankings_test_helpers:seats_by_name(Rankings))).

% https://en.wikipedia.org/wiki/2024_European_Parliament_election_in_Spain
% Spain elects 61 MEPs by closed-list d'Hondt in a single national
% constituency with no electoral threshold.
jefferson_dhondt_2024_spanish_european_parliament_case() ->
    Votes     = [ {pp, 5996627}, {psoe, 5291102}, {vox, 1688255},
                  {republics_now, 860660}, {sumar, 818015}, {salf, 803545},
                  {podemos, 578007}, {junts, 442140}, {ceus, 284888} ],
    Rankings  = consensus:jefferson_dhondt_rankings(Votes, 61),
    rankings_test_helpers:assert_seats(
        Rankings, 61,
        #{pp => 22, psoe => 20, vox => 6, republics_now => 3, sumar => 3,
          salf => 3, podemos => 2, junts => 1, ceus => 1}).

% https://en.wikipedia.org/wiki/2022_Danish_general_election
% The two Faroese North Atlantic mandates are allocated by d'Hondt.
jefferson_dhondt_2022_faroe_islands_case() ->
    Votes    = [ {union, 8198}, {soc_dem, 7659}, {republic, 4927},
                 {people, 4222}, {centre, 1217}, {progress, 936} ],
    Rankings = consensus:jefferson_dhondt_rankings(Votes, 2),
    rankings_test_helpers:assert_default_threshold_matches(
        Votes, 2, fun consensus:jefferson_dhondt_rankings/2,
        fun consensus:jefferson_dhondt_rankings/3),
    Expected = [ {union, 1, 0.60}, {soc_dem, 1, 0.56}, {republic, 0, 0.36},
                 {people, 0, 0.31}, {centre, 0, 0.08}, {progress, 0, 0.06} ],
    ?assertEqual(lists:sort(Expected), lists:sort(Rankings)).

% https://en.wikipedia.org/wiki/2022_Danish_general_election
% The two Greenlandic North Atlantic mandates are allocated by d'Hondt.
jefferson_dhondt_2022_greenland_case() ->
    Votes    = [ {siumut, 7424}, {ia, 4852}, {democrats, 3656},
                 {naleraq, 2416}, {atassut, 720}, {cooperation, 176} ],
    Rankings = consensus:jefferson_dhondt_rankings(Votes, 2),
    rankings_test_helpers:assert_default_threshold_matches(
        Votes, 2, fun consensus:jefferson_dhondt_rankings/2,
        fun consensus:jefferson_dhondt_rankings/3),
    Expected = [ {siumut, 1, 0.77}, {ia, 1, 0.50}, {democrats, 0, 0.37},
                 {naleraq, 0, 0.25}, {atassut, 0, 0.07}, {cooperation, 0, 0.01} ],
    ?assertEqual(lists:sort(Expected), lists:sort(Rankings)).

% https://en.wikipedia.org/wiki/2022_Danish_general_election
% Denmark proper elects 135 constituency seats by d'Hondt plus 40 Sainte-Lague
% levelling seats; this article gives only nationwide totals, and the levelling
% tier is not d'Hondt. A single nationwide d'Hondt run still matches every
% party's official 175-seat total within one seat. This two-tier, levelling
% allocation is out of scope for the d'Hondt functions, which model a single
% nationwide run.
jefferson_dhondt_2022_danish_general_election_case() ->
    Votes    = [ {sd, 971995}, {v, 470546}, {mod, 327699}, {gl, 293186},
                 {dd, 286796}, {la, 278656}, {con, 194820}, {rga, 181452},
                 {sl, 133931}, {nr, 129524}, {alt, 117567}, {dpp, 93428} ],
    Rankings  = consensus:jefferson_dhondt_rankings(Votes, 175, 0.02),
    SeatsByName = rankings_test_helpers:seats_by_name(Rankings),
    % Official totals, matched exactly by the nationwide run
    rankings_test_helpers:assert_seats(
        Rankings, 175,
        #{sd => 50, gl => 15, la => 14, dd => 14, con => 10, rga => 9,
          alt => 6, nr => 6}),
    % Official totals, within one seat of the nationwide run
    ?assert(maps:get(v,   SeatsByName) > 22 andalso maps:get(v,   SeatsByName) < 25),
    ?assert(maps:get(mod, SeatsByName) > 15 andalso maps:get(mod, SeatsByName) < 18),
    ?assert(maps:get(sl,  SeatsByName) > 5  andalso maps:get(sl,  SeatsByName) < 8),
    ?assert(maps:get(dpp, SeatsByName) > 3  andalso maps:get(dpp, SeatsByName) < 6).

% https://en.wikipedia.org/wiki/Faroe_Islands_(Folketing_constituency)
% The two Faroese North Atlantic mandates are allocated by d'Hondt.
jefferson_dhondt_2026_faroe_islands_case() ->
    Votes    = [ {soc_dem, 13088}, {union, 7342}, {people, 4383},
                 {republic, 3891}, {centre, 437} ],
    Rankings = consensus:jefferson_dhondt_rankings(Votes, 2),
    rankings_test_helpers:assert_default_threshold_matches(
        Votes, 2, fun consensus:jefferson_dhondt_rankings/2,
        fun consensus:jefferson_dhondt_rankings/3),
    Expected = [ {soc_dem, 1, 0.89}, {union, 1, 0.50}, {people, 0, 0.30},
                 {republic, 0, 0.26}, {centre, 0, 0.02} ],
    ?assertEqual(lists:sort(Expected), lists:sort(Rankings)).

% https://en.wikipedia.org/wiki/Greenland_(Folketing_constituency)
% The two Greenlandic North Atlantic mandates are allocated by d'Hondt.
jefferson_dhondt_2026_greenland_case() ->
    Votes    = [ {ia, 6133}, {naleraq, 5268}, {democrats, 3767},
                 {siumut, 3515}, {atassut, 2290} ],
    Rankings = consensus:jefferson_dhondt_rankings(Votes, 2),
    rankings_test_helpers:assert_default_threshold_matches(
        Votes, 2, fun consensus:jefferson_dhondt_rankings/2,
        fun consensus:jefferson_dhondt_rankings/3),
    Expected = [ {ia, 1, 0.58}, {naleraq, 1, 0.50}, {democrats, 0, 0.35},
                 {siumut, 0, 0.33}, {atassut, 0, 0.21} ],
    ?assertEqual(lists:sort(Expected), lists:sort(Rankings)).

% https://en.wikipedia.org/wiki/2026_Danish_general_election
% Denmark proper elects 135 constituency seats by d'Hondt plus 40 Sainte-Lague
% levelling seats; this article gives only nationwide totals, and the levelling
% tier is not d'Hondt. A single nationwide d'Hondt run still matches every
% party's official 175-seat total within one seat. This two-tier, levelling
% allocation is out of scope for the d'Hondt functions, which model a single
% nationwide run.
jefferson_dhondt_2026_danish_general_election_case() ->
    Votes    = [ {sd, 779252}, {gl, 413306}, {v, 361689}, {la, 334421},
                 {dpp, 324518}, {mod, 274775}, {con, 270749}, {rga, 226037},
                 {sl, 207442}, {dd, 205302}, {alt, 91770}, {cp, 75928} ],
    Rankings  = consensus:jefferson_dhondt_rankings(Votes, 175, 0.02),
    SeatsByName = rankings_test_helpers:seats_by_name(Rankings),
    % Official totals, matched exactly by the nationwide run
    rankings_test_helpers:assert_seats(
        Rankings, 175,
        #{v => 18, dpp => 16, con => 13, rga => 11, sl => 10, dd => 10}),
    % Official totals, within one seat of the nationwide run
    ?assert(maps:get(sd,  SeatsByName) > 37 andalso maps:get(sd,  SeatsByName) < 40),
    ?assert(maps:get(gl,  SeatsByName) > 19 andalso maps:get(gl,  SeatsByName) < 22),
    ?assert(maps:get(la,  SeatsByName) > 15 andalso maps:get(la,  SeatsByName) < 18),
    ?assert(maps:get(mod, SeatsByName) > 12 andalso maps:get(mod, SeatsByName) < 15),
    ?assert(maps:get(alt, SeatsByName) > 3  andalso maps:get(alt, SeatsByName) < 6),
    ?assert(maps:get(cp,  SeatsByName) > 2  andalso maps:get(cp,  SeatsByName) < 5).

% https://en.wikipedia.org/wiki/2023_Argentine_general_election
% The Chamber of Deputies elects 130 of its 257 members by d'Hondt in the 24
% electoral districts (the 23 provinces plus Buenos Aires City), each with a 3%
% electoral threshold. The article's "Results by province" table gives vote
% totals per district for up, lla, jxc and hxnp and amalgamates every other
% list under "Others"; merging that residual still reproduces the article's
% seat cells in every district except Rio Negro, whose residual is split here
% into the named Juntos Somos Rio Negro party and the remainder. The article's
% province-table total row sums to 131 seats while its own main table totals
% 130; the per-district cells are d'Hondt-consistent, so each case asserts the
% per-district cells it reports.
jefferson_dhondt_2023_argentine_district_cases_test_() ->
    [ {district_description(District), fun() -> assert_jefferson_seats(
        Votes, SeatsRenewed, 0.03, #{}, ExpectedSeatsByName) end}
      || {District, Votes, SeatsRenewed, ExpectedSeatsByName} <- argentine_district_cases() ].

argentine_district_cases() ->
    [
        {buenos_aires,
         [ {up, 4094665}, {lla, 2382198}, {jxc, 2484593}, {others, 405973} ],
         35, #{up => 16, lla => 9, jxc => 9, others => 1}},
        {buenos_aires_city,
         [ {up, 577225}, {lla, 377451}, {jxc, 782984}, {others, 99947} ],
         12, #{up => 4, lla => 2, jxc => 6}},
        {catamarca,
         [ {up, 102943}, {lla, 50657}, {jxc, 42415} ],
         2, #{up => 2}},
        {chaco,
         [ {up, 305229}, {lla, 192284}, {jxc, 172929} ],
         3, #{up => 1, lla => 1, jxc => 1}},
        {chubut,
         [ {up, 101885}, {lla, 118246}, {jxc, 71333}, {others, 18698} ],
         3, #{up => 1, lla => 1, jxc => 1}},
        {cordoba,
         [ {up, 286615}, {lla, 751428}, {jxc, 509837}, {hxnp, 679603},
           {others, 48057} ],
         9, #{up => 1, lla => 3, jxc => 2, hxnp => 3}},
        {corrientes,
         [ {up, 253334}, {lla, 178855}, {jxc, 222006} ],
         4, #{up => 2, lla => 1, jxc => 1}},
        {entre_rios,
         [ {up, 293605}, {lla, 174787}, {jxc, 269189} ],
         4, #{up => 2, lla => 1, jxc => 1}},
        {formosa,
         [ {up, 184028}, {lla, 101637}, {jxc, 53155} ],
         3, #{up => 2, lla => 1}},
        {jujuy,
         [ {up, 123971}, {lla, 152853}, {jxc, 96158}, {others, 28027} ],
         3, #{up => 1, lla => 1, jxc => 1}},
        {la_pampa,
         [ {up, 81094}, {jxc, 52574}, {others, 5796} ],
         2, #{up => 1, jxc => 1}},
        {la_rioja,
         [ {up, 93668}, {lla, 85794}, {jxc, 28656}, {hxnp, 17603} ],
         3, #{up => 2, lla => 1}},
        {mendoza,
         [ {up, 259263}, {lla, 462657}, {jxc, 287020}, {others, 62566} ],
         5, #{up => 1, lla => 3, jxc => 1}},
        {misiones,
         [ {up, 252335}, {jxc, 109856}, {others, 26776} ],
         4, #{up => 3, jxc => 1}},
        {neuquen,
         [ {up, 111828}, {lla, 136290}, {jxc, 79773}, {others, 56507} ],
         3, #{up => 1, lla => 1, jxc => 1}},
        {rio_negro,
         [ {up, 134429}, {lla, 126879}, {jxc, 67986}, {jsrn, 60259},
           {others, 19800} ],
         3, #{up => 1, lla => 1, jxc => 1}},
        {salta,
         [ {up, 280111}, {lla, 305397}, {jxc, 101188}, {hxnp, 40723},
           {others, 40487} ],
         4, #{up => 2, lla => 2}},
        {san_juan,
         [ {up, 154715}, {lla, 153576}, {jxc, 113662} ],
         3, #{up => 1, lla => 1, jxc => 1}},
        {san_luis,
         [ {up, 83178}, {lla, 125123}, {jxc, 71718}, {hxnp, 16323},
           {others, 8256} ],
         2, #{up => 1, lla => 1}},
        {santa_cruz,
         [ {up, 45182}, {jxc, 18595}, {others, 59391} ],
         2, #{up => 1, others => 1}},
        {santa_fe,
         [ {up, 584589}, {lla, 649809}, {jxc, 541534}, {hxnp, 184680},
           {others, 48469} ],
         10, #{up => 3, lla => 3, jxc => 3, hxnp => 1}},
        {santiago_del_estero,
         [ {up, 417016}, {jxc, 50115} ],
         4, #{up => 4}},
        {tierra_del_fuego,
         [ {up, 34206}, {lla, 30534}, {jxc, 13254}, {hxnp, 7908},
           {others, 14337} ],
         3, #{up => 2, lla => 1}},
        {tucuman,
         [ {up, 443377}, {lla, 286594}, {jxc, 171603}, {others, 43002} ],
         5, #{up => 3, lla => 1, jxc => 1}}
    ].

% https://en.wikipedia.org/wiki/Results_of_the_2026_Japanese_general_election
% The House of Representatives apportions its 176 proportional seats by
% d'Hondt separately in each of 11 PR blocks, with no per-block threshold.
% Each block table lists only the seat-relevant parties (any omitted minor
% list never reaches the last winning quotient) and gives exact vote totals
% alongside the published seat count.
%
% Forfeiture: a party can win fewer seats than d'Hondt grants it when it has
% too few list candidates to fill them, and the unfillable seats pass to the
% next-highest quotients. The LDP forfeited 14 seats (6 Southern Kanto,
% 5 Tokyo, 2 Hokuriku-Shin'etsu, 1 Chugoku) and Team Mirai forfeited 2 Kinki
% seats under the 10% dual-candidate rule. The five affected cases assert the
% pure d'Hondt result and then the published post-forfeiture result by
% capping each affected party at its number of fillable list candidates.
jefferson_dhondt_2026_japanese_pr_block_cases_test_() ->
    lists:flatmap(
        fun({Block, Votes, SeatsRenewed, Runs}) ->
            [ {run_description(Block, Phase),
               fun() -> assert_jefferson_seats(
                   Votes, SeatsRenewed, 0.0, MaxSeatsByName, ExpectedSeatsByName) end}
              || {Phase, MaxSeatsByName, ExpectedSeatsByName} <- Runs ]
        end,
        japanese_pr_block_cases()).

% The LDP could fill only 5 list seats, so its 6th seat goes to Ishin.
japanese_pr_block_cases() ->
    [
        {chugoku,
         [ {ldp, 1297078}, {cra, 620211}, {dpfp, 273263}, {sansei, 252036},
           {ishin, 213428} ],
         10, [ {pure, #{}, #{ldp => 6, cra => 2, dpfp => 1, sansei => 1, ishin => 0}},
               {forfeiture, #{ldp => 5}, #{ldp => 5, cra => 2, dpfp => 1, sansei => 1, ishin => 1}} ]},
        {hokkaido,
         [ {ldp, 911742}, {cra, 605889}, {dpfp, 218850} ],
         8, [ {pure, #{}, #{ldp => 4, cra => 3, dpfp => 1}} ]},
        % The LDP could fill only 3 list seats, so its 4th and 5th go to CRA.
        {hokuriku_shinetsu,
         [ {ldp, 1387716}, {cra, 651331}, {dpfp, 322739}, {sansei, 285897},
           {ishin, 231686} ],
         10, [ {pure, #{}, #{ldp => 5, cra => 2, dpfp => 1, sansei => 1, ishin => 1}},
               {forfeiture, #{ldp => 3}, #{ldp => 3, cra => 4, dpfp => 1, sansei => 1, ishin => 1}} ]},
        % Team Mirai could fill no Kinki list seat (10% rule), so its two seats
        % go to Ishin and CRA.
        {kinki,
         [ {ldp, 2842876}, {ishin, 2168635}, {cra, 1332752}, {dpfp, 651785},
           {sansei, 631179}, {mirai, 553496}, {jcp, 480188} ],
         28, [ {pure, #{}, #{ldp => 10, ishin => 7, cra => 4, dpfp => 2,
                              sansei => 2, mirai => 2, jcp => 1}},
               {forfeiture, #{mirai => 0}, #{ldp => 10, ishin => 8, cra => 5,
                                            dpfp => 2, sansei => 2, mirai => 0, jcp => 1}} ]},
        {kyushu,
         [ {ldp, 2516334}, {cra, 1181744}, {sansei, 519904}, {dpfp, 505129},
           {mirai, 375396}, {ishin, 374428} ],
         20, [ {pure, #{}, #{ldp => 10, cra => 4, sansei => 2, dpfp => 2,
                             mirai => 1, ishin => 1}} ]},
        {northern_kanto,
         [ {ldp, 2256845}, {cra, 1174717}, {dpfp, 626695}, {sansei, 506071},
           {mirai, 467561}, {ishin, 326128}, {jcp, 254497} ],
         19, [ {pure, #{}, #{ldp => 8, cra => 4, dpfp => 2, sansei => 2,
                             mirai => 1, ishin => 1, jcp => 1}} ]},
        {shikoku,
         [ {ldp, 691402}, {cra, 299777}, {dpfp, 213661} ],
         6, [ {pure, #{}, #{ldp => 4, cra => 1, dpfp => 1}} ]},
        % The LDP could fill only 4 list seats, so its 6 forfeited seats go to
        % CRA (2), DPFP, Mirai, Ishin and Reiwa.
        {southern_kanto,
         [ {ldp, 2637938}, {cra, 1424763}, {dpfp, 788564}, {mirai, 682469},
           {sansei, 531288}, {ishin, 463287}, {jcp, 314224}, {reiwa, 202655} ],
         23, [ {pure, #{}, #{ldp => 10, cra => 5, dpfp => 2, mirai => 2,
                             sansei => 2, ishin => 1, jcp => 1, reiwa => 0}},
               {forfeiture, #{ldp => 4}, #{ldp => 4, cra => 7, dpfp => 3,
                                          mirai => 3, sansei => 2, ishin => 2,
                                          jcp => 1, reiwa => 1}} ]},
        {tohoku,
         [ {ldp, 1612576}, {cra, 828883}, {dpfp, 389503}, {sansei, 278410},
           {mirai, 234050} ],
         12, [ {pure, #{}, #{ldp => 6, cra => 3, dpfp => 1, sansei => 1, mirai => 1}} ]},
        {tokai,
         [ {ldp, 2628008}, {cra, 1199580}, {dpfp, 836102}, {sansei, 527291},
           {mirai, 478316}, {ishin, 400964} ],
         21, [ {pure, #{}, #{ldp => 10, cra => 4, dpfp => 3, sansei => 2,
                             mirai => 1, ishin => 1}} ]},
        % The LDP could fill only 3 list seats, so its 5 forfeited seats go to
        % CRA (2), Mirai, DPFP and Sansei.
        {tokyo,
         [ {ldp, 2243625}, {cra, 1119155}, {mirai, 887849}, {dpfp, 746660},
           {sansei, 427028}, {jcp, 407146}, {ishin, 384487} ],
         19, [ {pure, #{}, #{ldp => 8, cra => 3, mirai => 3, dpfp => 2,
                             sansei => 1, jcp => 1, ishin => 1}},
               {forfeiture, #{ldp => 3}, #{ldp => 3, cra => 5, mirai => 4,
                                          dpfp => 3, sansei => 2, jcp => 1,
                                          ishin => 1}} ]}
    ].

%%% PRIVATE FUNCTIONS

assert_jefferson_seats(Votes, SeatsRenewed, Threshold, MaxSeatsByName, ExpectedSeatsByName) ->
    Rankings = consensus:jefferson_dhondt_rankings(Votes, SeatsRenewed, Threshold, MaxSeatsByName),
    rankings_test_helpers:assert_seats(Rankings, SeatsRenewed, ExpectedSeatsByName).

district_description(District) ->
    atom_to_list(District).

run_description(Block, Phase) ->
    atom_to_list(Block) ++ " " ++ atom_to_list(Phase).