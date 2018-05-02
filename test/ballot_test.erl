-module('ballot_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").
-include_lib("elections.hrl").

ballot_setup()     -> ok.
ballot_teardown(_) -> ok.

ballot_test_() ->
    {setup, fun ballot_setup/0,
            fun ballot_teardown/1,
            [
                fun get_tail_names_case/0,
                fun has_top_choice_case/0,
                fun make_and_candidates_case/0,
                fun only_multis_case/0
            ]
    }.

get_tail_names_case() ->
    B = ballot:make([a,b,c]),
    ?assertEqual([b,c], ballot:get_tail_names(B)).

has_top_choice_case() ->
    B = ballot:make([a,b,c]),
    ?assert(ballot:has_top_choice(a, B)),
    ?assertNot(ballot:has_top_choice(b, B)),
    ?assertNot(ballot:has_top_choice(c, B)).

make_and_candidates_case() ->
    B = ballot:make([a,b,c]),
    Cs = ballot:candidates(B),
    ?assertEqual([a,b,c], lists:map(fun candidate:name/1, Cs)).

only_multis_case() ->
    B1 = ballot:make([a,b,c]),
    B2 = ballot:make([a]),
    ?assertEqual([B1], ballot:only_multis([B1, B2])).

%%% PRIVATE FUNCTIONS

