-module('candidate_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

candidate_setup()     -> ok.
candidate_teardown(_) -> ok.

candidate_test_() ->
    {setup, fun candidate_setup/0,
            fun candidate_teardown/1,
            [
                fun make_case/0,
                fun make_partisan_case/0
            ]
    }.

make_case() ->
    Cs = [candidate:make(I) || I <- [a, b, c]],
    ?assertEqual([a, b, c], lists:map(fun candidate:name/1, Cs)).

make_partisan_case() ->
    Input = [{a, <<"lib">>}, {b, <<"con">>}, {c, <<"lab">>}],
    Cs = [candidate:make(I) || I <- Input],
    ?assertEqual([a, b, c], lists:map(fun candidate:name/1, Cs)),
    ?assertEqual([<<"lib">>, <<"con">>, <<"lab">>], lists:map(fun candidate:party/1, Cs)).

%%% PRIVATE FUNCTIONS

