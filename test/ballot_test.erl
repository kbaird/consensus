-module('ballot_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

ballot_test_() ->
    [
        fun get_2nd_choice_name_case/0,
        fun get_tail_names_case/0,
        fun has_top_choice_case/0,
        fun make_and_candidates_case/0,
        fun make_from_candidates_case/0,
        fun make_partisan_case/0,
        fun only_multis_case/0
    ].

get_2nd_choice_name_case() ->
    B = abc_ballot(),
    ?assertEqual(b, ballot:get_2nd_choice_name(B)).

get_tail_names_case() ->
    B = abc_ballot(),
    ?assertEqual([b, c], ballot:get_tail_names(B)).

has_top_choice_case() ->
    B = abc_ballot(),
    ?assert(ballot:has_top_choice(a, B)),
    ?assertNot(ballot:has_top_choice(b, B)),
    ?assertNot(ballot:has_top_choice(c, B)).

make_and_candidates_case() ->
    Cs = ballot:candidates(abc_ballot()),
    assert_abc_names(Cs).

make_from_candidates_case() ->
    Input = partisan_input(),
    Cs2 = candidates_round_trip(Input),
    assert_abc_names(Cs2),
    ?assertEqual(partisan_parties(), parties(Cs2)).

make_partisan_case() ->
    Cs = ballot:candidates(ballot:make(partisan_input())),
    assert_abc_names(Cs),
    ?assertEqual(partisan_parties(), parties(Cs)).

only_multis_case() ->
    B1 = abc_ballot(),
    B2 = ballot:make([a]),
    ?assertEqual([B1], ballot:only_multis([B1, B2])).

%%% PRIVATE FUNCTIONS

abc_ballot() -> ballot:make([a, b, c]).

assert_abc_names(Cs) -> ?assertEqual([a, b, c], names(Cs)).

partisan_input() -> [{a, <<"lib">>}, {b, <<"con">>}, {c, <<"lab">>}].

partisan_parties() -> [<<"lib">>, <<"con">>, <<"lab">>].

candidates_round_trip(Input) ->
    Cs = ballot:candidates(ballot:make(Input)),
    ballot:candidates(ballot:make(Cs)).

names(Cs) -> lists:map(fun candidate:name/1, Cs).

parties(Cs) -> lists:map(fun candidate:party/1, Cs).

