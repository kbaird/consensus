-module('label_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

label_test_() ->
    [
        fun cabinet_synonyms_normalize_to_canonical_case/0,
        fun canonical_labels_are_unchanged_case/0,
        fun borda_synonym_normalizes_to_canonical_case/0,
        fun unknown_labels_are_unchanged_case/0
    ].

cabinet_synonyms_normalize_to_canonical_case() ->
    ?assertEqual(bp,  label:normalize(bargaining_proposition)),
    ?assertEqual(mr,  label:normalize(minimal_range)),
    ?assertEqual(mr,  label:normalize(minimum_range)),
    ?assertEqual(ms,  label:normalize(minimal_size)),
    ?assertEqual(ms,  label:normalize(minimum_size)),
    ?assertEqual(mwc, label:normalize(minimal_winning_coalition)),
    ?assertEqual(mwc, label:normalize(minimum_winning_coalition)),
    ?assertEqual(mcw, label:normalize(minimal_connected_winning)),
    ?assertEqual(mcw, label:normalize(minimum_connected_winning)),
    ?assertEqual(pvc, label:normalize(policy_viable_coalition)).

canonical_labels_are_unchanged_case() ->
    ?assertEqual(bp,    label:normalize(bp)),
    ?assertEqual(mr,    label:normalize(mr)),
    ?assertEqual(ms,    label:normalize(ms)),
    ?assertEqual(mwc,   label:normalize(mwc)),
    ?assertEqual(mcw,   label:normalize(mcw)),
    ?assertEqual(pvc,   label:normalize(pvc)),
    ?assertEqual(base0, label:normalize(base0)),
    ?assertEqual(base1, label:normalize(base1)),
    ?assertEqual(nauru, label:normalize(nauru)).

borda_synonym_normalizes_to_canonical_case() ->
    ?assertEqual(nauru, label:normalize(dowdell)).

unknown_labels_are_unchanged_case() ->
    ?assertEqual(bogus, label:normalize(bogus)).
