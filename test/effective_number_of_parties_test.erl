-module('effective_number_of_parties_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

% effective # of parties

effective_num_parties_setup()     -> ok.
effective_num_parties_teardown(_) -> ok.

effective_num_parties_test_() ->
    {setup, fun effective_num_parties_setup/0,
            fun effective_num_parties_teardown/1,
            [
                fun effective_num_parties_dictatorship_case/0,
                fun effective_num_parties_even_two_case/0,
                fun effective_num_parties_even_ten_case/0,
                fun effective_num_parties_skewed_two_case/0,
                fun effective_num_parties_skewed_three_case/0
            ]
    }.

effective_num_parties_dictatorship_case() ->
    Count = consensus:effective_num_parties(single_party_state()),
    ?assertEqual(1.0, Count).

effective_num_parties_even_two_case() ->
    Count = consensus:effective_num_parties(even_two_state()),
    ?assertEqual(2.0, Count).

effective_num_parties_even_ten_case() ->
    Count = consensus:effective_num_parties(even_ten_state()),
    ?assert(Count > 9.95),
    ?assert(Count < 10.05).

effective_num_parties_skewed_two_case() ->
    Count = consensus:effective_num_parties(skewed_two_state()),
    ?assert(Count > 1.65),
    ?assert(Count < 1.75).

effective_num_parties_skewed_three_case() ->
    Count = consensus:effective_num_parties(skewed_three_state()),
    ?assert(Count > 2.55),
    ?assert(Count < 2.65).

%%% PRIVATE FUNCTIONS

single_party_state() -> [ {ccp, 1} ].
even_two_state()     -> [ {left, 0.5}, {right, 0.5} ].
even_ten_state()     -> [ {a, 0.1}, {b, 0.1}, {c, 0.1}, {d, 0.1}, {e, 0.1},
                          {f, 0.1}, {g, 0.1}, {h, 0.1}, {i, 0.1}, {j, 0.1} ].
skewed_two_state()   -> [ {strong, 0.7}, {weak, 0.3} ].
skewed_three_state() -> [ {strong, 0.45}, {middle, 0.4}, {weak, 0.15} ].

