-module('cabinet_composition_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

% cabinet_composition

cabinet_composition_test_() ->
    % Cf. Lijphart, Arend, _Patterns of Democracy_, 1999. pg93.
    [
        fun cabinet_composition_mwc_case/0,
        fun cabinet_composition_ms_case/0,
        fun cabinet_composition_bp_case/0,
        fun cabinet_composition_mr_case/0,
        fun cabinet_composition_mcw_case/0,
        fun cabinet_composition_pvc_case/0
    ].

cabinet_composition_mwc_case() ->
    Cabinet = consensus:cabinet_composition(minimal_winning_coalition, parliament()),
    Cabinet = consensus:cabinet_composition(minimum_winning_coalition, parliament()),
    Cabinet = consensus:cabinet_composition(mwc, parliament()),
    ?assertEqual([abc(), ade(), bcd(), be(), ce()], Cabinet).

cabinet_composition_ms_case() ->
    Cabinet = consensus:cabinet_composition(minimal_size, parliament()),
    Cabinet = consensus:cabinet_composition(minimum_size, parliament()),
    Cabinet = consensus:cabinet_composition(ms, parliament()),
    ?assertEqual([ade()], Cabinet).

cabinet_composition_bp_case() ->
    Cabinet = consensus:cabinet_composition(bargaining_proposition, parliament()),
    Cabinet = consensus:cabinet_composition(bp, parliament()),
    ?assertEqual([be(), ce()], Cabinet).

cabinet_composition_mr_case() ->
    Cabinet = consensus:cabinet_composition(minimal_range, parliament()),
    Cabinet = consensus:cabinet_composition(minimum_range, parliament()),
    Cabinet = consensus:cabinet_composition(mr, parliament()),
    ?assertEqual(mr_pvc_cabinet(), Cabinet).

cabinet_composition_mcw_case() ->
    AllPartyNames = all_party_names(),
    Cabinet = consensus:cabinet_composition(minimal_connected_winning, parliament(), AllPartyNames),
    Cabinet = consensus:cabinet_composition(minimum_connected_winning, parliament(), AllPartyNames),
    Cabinet = consensus:cabinet_composition(mcw, parliament(), AllPartyNames),
    ?assertEqual([abc(), bcd(), cde()], Cabinet).

cabinet_composition_pvc_case() ->
    Cabinet = consensus:cabinet_composition(policy_viable_coalition, parliament()),
    Cabinet = consensus:cabinet_composition(pvc, parliament()),
    ?assertEqual(mr_pvc_cabinet(), Cabinet).

%%% PRIVATE FUNCTIONS

abc() -> [<<"a">>, <<"b">>, <<"c">>].

ade() -> [<<"a">>, <<"d">>, <<"e">>].

bcd() -> [<<"b">>, <<"c">>, <<"d">>].

be() -> [<<"b">>, <<"e">>].

ce() -> [<<"c">>, <<"e">>].

cde() -> [<<"c">>, <<"d">>, <<"e">>].

mr_pvc_cabinet() -> [abc(), bcd(), ce()].

all_party_names() -> [<<"a">>, <<"b">>, <<"c">>, <<"d">>, <<"e">>].

parliament() -> [
    consensus_party:make(<<"a">>, 8),
    consensus_party:make(<<"b">>, 21),
    consensus_party:make(<<"c">>, 26),
    consensus_party:make(<<"d">>, 12),
    consensus_party:make(<<"e">>, 33)
].
