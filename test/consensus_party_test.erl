-module('consensus_party_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

consensus_party_test_() ->
    [
        fun make_3_stores_fraction_vote_share_case/0,
        fun make_3_does_not_infer_percent_case/0,
        fun make_percent_3_converts_to_fraction_case/0,
        fun make_percent_3_matches_make_3_case/0
    ].

make_3_stores_fraction_vote_share_case() ->
    PartyResult = consensus_party:make(a, 1, 0.42),
    ?assertEqual(0.42, consensus_party:vote_share(PartyResult)).

make_3_does_not_infer_percent_case() ->
    PartyResult = consensus_party:make(a, 1, 42.0),
    ?assertEqual(42.0, consensus_party:vote_share(PartyResult)).

make_percent_3_converts_to_fraction_case() ->
    PartyResult = consensus_party:make_percent(a, 1, 42.0),
    ?assertEqual(0.42, consensus_party:vote_share(PartyResult)).

make_percent_3_matches_make_3_case() ->
    ?assertEqual(consensus_party:make(a, 1, 0.42),
                 consensus_party:make_percent(a, 1, 42.0)).
