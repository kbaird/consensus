-module('jefferson_dhondt_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

jefferson_dhondt_setup()     -> ok.
jefferson_dhondt_teardown(_) -> ok.

% https://en.wikipedia.org/wiki/D%27Hondt_method#Example
jefferson_dhondt_test_() ->
    {setup, fun jefferson_dhondt_setup/0,
            fun jefferson_dhondt_teardown/1,
            [
                fun jefferson_dhondt_basic_case/0,
                fun jefferson_dhondt_2024_french_european_parliament_case/0,
                fun jefferson_dhondt_2024_spanish_european_parliament_case/0,
                fun jefferson_dhondt_2022_faroe_islands_case/0,
                fun jefferson_dhondt_2022_greenland_case/0,
                fun jefferson_dhondt_2022_danish_general_election_case/0,
                fun jefferson_dhondt_2026_faroe_islands_case/0,
                fun jefferson_dhondt_2026_greenland_case/0,
                fun jefferson_dhondt_2026_danish_general_election_case/0
            ]
    }.

jefferson_dhondt_basic_case() ->
    Votes    = [ {a, 100000}, {b, 80000}, {c, 30000}, {d, 20000} ],
    Rankings = consensus:jefferson_dhondt_rankings(Votes, 8),
    Rankings = consensus:jefferson_dhondt_rankings(Votes, 8, 0.0),
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
    SeatByName = maps:from_list([{Party, Seats} || {Party, Seats, _} <- Rankings]),
    ?assertEqual(81, lists:sum(maps:values(SeatByName))),
    ?assertEqual(7,  map_size(SeatByName)),
    ?assertEqual(30, maps:get(rn,         SeatByName)),
    ?assertEqual(13, maps:get(ensemble,   SeatByName)),
    ?assertEqual(13, maps:get(socialist,  SeatByName)),
    ?assertEqual(9,  maps:get(lfi,        SeatByName)),
    ?assertEqual(6,  maps:get(republicans, SeatByName)),
    ?assertEqual(5,  maps:get(ecologists, SeatByName)),
    ?assertEqual(5,  maps:get(reconquete, SeatByName)).

% https://en.wikipedia.org/wiki/2024_European_Parliament_election_in_Spain
% Spain elects 61 MEPs by closed-list d'Hondt in a single national
% constituency with no electoral threshold.
jefferson_dhondt_2024_spanish_european_parliament_case() ->
    Votes     = [ {pp, 5996627}, {psoe, 5291102}, {vox, 1688255},
                  {republics_now, 860660}, {sumar, 818015}, {salf, 803545},
                  {podemos, 578007}, {junts, 442140}, {ceus, 284888} ],
    Rankings  = consensus:jefferson_dhondt_rankings(Votes, 61),
    SeatByName = maps:from_list([{Party, Seats} || {Party, Seats, _} <- Rankings]),
    ?assertEqual(61, lists:sum(maps:values(SeatByName))),
    ?assertEqual(22, maps:get(pp,           SeatByName)),
    ?assertEqual(20, maps:get(psoe,         SeatByName)),
    ?assertEqual(6,  maps:get(vox,          SeatByName)),
    ?assertEqual(3,  maps:get(republics_now, SeatByName)),
    ?assertEqual(3,  maps:get(sumar,        SeatByName)),
    ?assertEqual(3,  maps:get(salf,         SeatByName)),
    ?assertEqual(2,  maps:get(podemos,      SeatByName)),
    ?assertEqual(1,  maps:get(junts,        SeatByName)),
    ?assertEqual(1,  maps:get(ceus,         SeatByName)).

% https://en.wikipedia.org/wiki/2022_Danish_general_election
% The two Faroese North Atlantic mandates are allocated by d'Hondt.
jefferson_dhondt_2022_faroe_islands_case() ->
    Votes    = [ {union, 8198}, {soc_dem, 7659}, {republic, 4927},
                 {people, 4222}, {centre, 1217}, {progress, 936} ],
    Rankings = consensus:jefferson_dhondt_rankings(Votes, 2),
    Rankings = consensus:jefferson_dhondt_rankings(Votes, 2, 0.0),
    Expected = [ {union, 1, 0.60}, {soc_dem, 1, 0.56}, {republic, 0, 0.36},
                 {people, 0, 0.31}, {centre, 0, 0.08}, {progress, 0, 0.06} ],
    ?assertEqual(lists:sort(Expected), lists:sort(Rankings)).

% https://en.wikipedia.org/wiki/2022_Danish_general_election
% The two Greenlandic North Atlantic mandates are allocated by d'Hondt.
jefferson_dhondt_2022_greenland_case() ->
    Votes    = [ {siumut, 7424}, {ia, 4852}, {democrats, 3656},
                 {naleraq, 2416}, {atassut, 720}, {cooperation, 176} ],
    Rankings = consensus:jefferson_dhondt_rankings(Votes, 2),
    Rankings = consensus:jefferson_dhondt_rankings(Votes, 2, 0.0),
    Expected = [ {siumut, 1, 0.77}, {ia, 1, 0.50}, {democrats, 0, 0.37},
                 {naleraq, 0, 0.25}, {atassut, 0, 0.07}, {cooperation, 0, 0.01} ],
    ?assertEqual(lists:sort(Expected), lists:sort(Rankings)).

% https://en.wikipedia.org/wiki/2022_Danish_general_election
% Denmark proper elects 135 constituency seats by d'Hondt plus 40 Sainte-Lague
% levelling seats; this article gives only nationwide totals, and the levelling
% tier is not d'Hondt. A single nationwide d'Hondt run still matches every
% party's official 175-seat total within one seat.
jefferson_dhondt_2022_danish_general_election_case() ->
    Votes    = [ {sd, 971995}, {v, 470546}, {mod, 327699}, {gl, 293186},
                 {dd, 286796}, {la, 278656}, {con, 194820}, {rga, 181452},
                 {sl, 133931}, {nr, 129524}, {alt, 117567}, {dpp, 93428} ],
    Rankings  = consensus:jefferson_dhondt_rankings(Votes, 175, 0.02),
    SeatByName = maps:from_list([{Party, Seats} || {Party, Seats, _} <- Rankings]),
    ?assertEqual(175, lists:sum(maps:values(SeatByName))),
    % Official totals, matched exactly by the nationwide run
    ?assertEqual(50, maps:get(sd,  SeatByName)),
    ?assertEqual(15, maps:get(gl,  SeatByName)),
    ?assertEqual(14, maps:get(la,  SeatByName)),
    ?assertEqual(14, maps:get(dd,  SeatByName)),
    ?assertEqual(10, maps:get(con, SeatByName)),
    ?assertEqual(9,  maps:get(rga, SeatByName)),
    ?assertEqual(6,  maps:get(alt, SeatByName)),
    ?assertEqual(6,  maps:get(nr,  SeatByName)),
    % Official totals, within one seat of the nationwide run
    ?assert(maps:get(v,   SeatByName) > 22 andalso maps:get(v,   SeatByName) < 25),
    ?assert(maps:get(mod, SeatByName) > 15 andalso maps:get(mod, SeatByName) < 18),
    ?assert(maps:get(sl,  SeatByName) > 5  andalso maps:get(sl,  SeatByName) < 8),
    ?assert(maps:get(dpp, SeatByName) > 3  andalso maps:get(dpp, SeatByName) < 6).

% https://en.wikipedia.org/wiki/Faroe_Islands_(Folketing_constituency)
% The two Faroese North Atlantic mandates are allocated by d'Hondt.
jefferson_dhondt_2026_faroe_islands_case() ->
    Votes    = [ {soc_dem, 13088}, {union, 7342}, {people, 4383},
                 {republic, 3891}, {centre, 437} ],
    Rankings = consensus:jefferson_dhondt_rankings(Votes, 2),
    Rankings = consensus:jefferson_dhondt_rankings(Votes, 2, 0.0),
    Expected = [ {soc_dem, 1, 0.89}, {union, 1, 0.50}, {people, 0, 0.30},
                 {republic, 0, 0.26}, {centre, 0, 0.02} ],
    ?assertEqual(lists:sort(Expected), lists:sort(Rankings)).

% https://en.wikipedia.org/wiki/Greenland_(Folketing_constituency)
% The two Greenlandic North Atlantic mandates are allocated by d'Hondt.
jefferson_dhondt_2026_greenland_case() ->
    Votes    = [ {ia, 6133}, {naleraq, 5268}, {democrats, 3767},
                 {siumut, 3515}, {atassut, 2290} ],
    Rankings = consensus:jefferson_dhondt_rankings(Votes, 2),
    Rankings = consensus:jefferson_dhondt_rankings(Votes, 2, 0.0),
    Expected = [ {ia, 1, 0.58}, {naleraq, 1, 0.50}, {democrats, 0, 0.35},
                 {siumut, 0, 0.33}, {atassut, 0, 0.21} ],
    ?assertEqual(lists:sort(Expected), lists:sort(Rankings)).

% https://en.wikipedia.org/wiki/2026_Danish_general_election
% Denmark proper elects 135 constituency seats by d'Hondt plus 40 Sainte-Lague
% levelling seats; this article gives only nationwide totals, and the levelling
% tier is not d'Hondt. A single nationwide d'Hondt run still matches every
% party's official 175-seat total within one seat.
jefferson_dhondt_2026_danish_general_election_case() ->
    Votes    = [ {sd, 779252}, {gl, 413306}, {v, 361689}, {la, 334421},
                 {dpp, 324518}, {mod, 274775}, {con, 270749}, {rga, 226037},
                 {sl, 207442}, {dd, 205302}, {alt, 91770}, {cp, 75928} ],
    Rankings  = consensus:jefferson_dhondt_rankings(Votes, 175, 0.02),
    SeatByName = maps:from_list([{Party, Seats} || {Party, Seats, _} <- Rankings]),
    ?assertEqual(175, lists:sum(maps:values(SeatByName))),
    % Official totals, matched exactly by the nationwide run
    ?assertEqual(18, maps:get(v,   SeatByName)),
    ?assertEqual(16, maps:get(dpp, SeatByName)),
    ?assertEqual(13, maps:get(con, SeatByName)),
    ?assertEqual(11, maps:get(rga, SeatByName)),
    ?assertEqual(10, maps:get(sl,  SeatByName)),
    ?assertEqual(10, maps:get(dd,  SeatByName)),
    % Official totals, within one seat of the nationwide run
    ?assert(maps:get(sd,  SeatByName) > 37 andalso maps:get(sd,  SeatByName) < 40),
    ?assert(maps:get(gl,  SeatByName) > 19 andalso maps:get(gl,  SeatByName) < 22),
    ?assert(maps:get(la,  SeatByName) > 15 andalso maps:get(la,  SeatByName) < 18),
    ?assert(maps:get(mod, SeatByName) > 12 andalso maps:get(mod, SeatByName) < 15),
    ?assert(maps:get(alt, SeatByName) > 3  andalso maps:get(alt, SeatByName) < 6),
    ?assert(maps:get(cp,  SeatByName) > 2  andalso maps:get(cp,  SeatByName) < 5).

%%% PRIVATE FUNCTIONS

