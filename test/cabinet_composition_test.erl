-module('cabinet_composition_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

% cabinet_composition

cabinet_composition_setup()     -> ok.
cabinet_composition_teardown(_) -> ok.

cabinet_composition_test_() ->
    % Cf. Lijphart, Arend, _Patterns of Democracy_, 1999. pg93.
    {setup, fun cabinet_composition_setup/0,
            fun cabinet_composition_teardown/1,
            [
                fun cabinet_composition_mwc_case/0,
                fun cabinet_composition_ms_case/0,
                fun cabinet_composition_bp_case/0,
                fun cabinet_composition_mr_case/0,
                fun cabinet_composition_mcw_case/0,
                fun cabinet_composition_pvc_case/0
            ]
    }.

cabinet_composition_mwc_case() ->
    Cabinet = consensus:cabinet_composition(minimal_winning_coalition, parliament()),
    Cabinet = consensus:cabinet_composition(minimum_winning_coalition, parliament()),
    Cabinet = consensus:cabinet_composition(mwc, parliament()),
    ?assertEqual([[a, b, c], [a, d, e], [b, c, d], [b, e], [c, e]], Cabinet).

cabinet_composition_ms_case() ->
    Cabinet = consensus:cabinet_composition(minimal_size, parliament()),
    Cabinet = consensus:cabinet_composition(minimum_size, parliament()),
    Cabinet = consensus:cabinet_composition(ms, parliament()),
    ?assertEqual([[a, d, e]], Cabinet).

cabinet_composition_bp_case() ->
    Cabinet = consensus:cabinet_composition(bargaining_proposition, parliament()),
    Cabinet = consensus:cabinet_composition(bp, parliament()),
    ?assertEqual([[b, e], [c, e]], Cabinet).

cabinet_composition_mr_case() ->
    Cabinet = consensus:cabinet_composition(minimal_range, parliament()),
    Cabinet = consensus:cabinet_composition(minimum_range, parliament()),
    Cabinet = consensus:cabinet_composition(mr, parliament()),
    ?assertEqual([[a, b, c], [b, c, d], [c, e]], Cabinet).

cabinet_composition_mcw_case() ->
    Cabinet = consensus:cabinet_composition(minimal_connected_winning, parliament()),
    Cabinet = consensus:cabinet_composition(minimum_connected_winning, parliament()),
    Cabinet = consensus:cabinet_composition(mcw, parliament()),
    ?assertEqual([[a, b, c], [b, c, d], [c, d, e]], Cabinet).

cabinet_composition_pvc_case() ->
    Cabinet = consensus:cabinet_composition(policy_viable_coalition, parliament()),
    Cabinet = consensus:cabinet_composition(pvc, parliament()),
    ?assertEqual([[a, b, c], [b, c, d], [c, e]], Cabinet).

%%% PRIVATE FUNCTIONS

parliament() -> [
    consensus_party:make(a, 8),
    consensus_party:make(b, 21),
    consensus_party:make(c, 26),
    consensus_party:make(d, 12),
    consensus_party:make(e, 33)
].
