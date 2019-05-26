-module(ballot).

%% API exports
-export([
    make/1,
    make_mmp/2,
    candidates/1,
    get_2nd_choice_name/1,
    get_tail_names/1,
    has_top_choice/2,
    only_multis/1,
    party/1
]).

-include("include/elections.hrl").
-include("include/parties.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec make([name(), ...] | [candidate(), ...]) -> ballot().
make(CandidatesOrNames) ->
    Candidates = lists:map(fun ensure_candidate/1, CandidatesOrNames),
    #ballot{candidates = Candidates}.

-spec make_mmp([name(), ...] | [candidate(), ...], party_name()) -> mmp_ballot().
make_mmp(CandidatesOrNames, PartyListChoice) ->
    Candidates = lists:map(fun ensure_candidate/1, CandidatesOrNames),
    #mmp_ballot{candidates = Candidates, party_list_choice = PartyListChoice}.

-spec candidates(ballot() | mmp_ballot()) -> [candidate(), ...].
candidates(#ballot{candidates = Candidates}) -> Candidates;
candidates(#mmp_ballot{candidates = Candidates}) -> Candidates.

-spec get_2nd_choice_name(ballot()) -> name().
get_2nd_choice_name(Ballot) ->
    hd(get_tail_names(Ballot)).

-spec get_tail_names(ballot()) -> [name()].
get_tail_names(Ballot) ->
    [ _ | Cs ] = candidates(Ballot),
    [ candidate:name(C) || C <- Cs ].

-spec has_top_choice(name(), ballot()) -> boolean().
has_top_choice(Name, Ballot) ->
   Names = lists:map(fun candidate:name/1, candidates(Ballot)),
   hd(Names) =:= Name.

-spec only_multis([ballot()]) -> [ballot()].
only_multis(Ballots) ->
    lists:filter(fun(B) -> length(candidates(B)) > 1 end, Ballots).

-spec party(mmp_ballot()) -> party_name().
party(#mmp_ballot{party_list_choice = PartyListChoice}) -> PartyListChoice.

%%====================================================================
%% Internal functions
%%====================================================================
ensure_candidate(X) ->
    case candidate:is_candidate(X) of
        true -> X;
        _ -> candidate:make(X)
    end.
