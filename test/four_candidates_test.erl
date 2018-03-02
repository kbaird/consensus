-module('four_candidates_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").
-include_lib("elections.hrl").

four_candidates_setup()     -> ok.
four_candidates_teardown(_) -> ok.

four_candidates_test_() ->
    {setup, fun four_candidates_setup/0,
            fun four_candidates_teardown/1,
            [
                fun four_candidates_divergent_case/0
            ]
    }.

four_candidates_divergent_case() ->
    Ballot1  = ballot:make([a, b, c]),
    Ballot2  = ballot:make([b, a, c]),
    Ballot3  = ballot:make([b, c, a]),
    Ballot4  = ballot:make([c, b, a]),
    Ballots  = lists:flatten([
        lists:duplicate(33, Ballot1),
        lists:duplicate(16, Ballot2),
        lists:duplicate(16, Ballot3),
        lists:duplicate(35, Ballot4)
    ]),
    Rankings = consensus:condorcet_rankings(Ballots),
    ?assertEqual([b, c, a], Rankings).

%%% PRIVATE FUNCTIONS

