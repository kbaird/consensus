-module(ballot).

%% API exports
-export([
    make/1,
    candidates/1,
    get_2nd_choice_name/1,
    has_top_choice/2,
    only_multis/1
]).

-include("elections.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec make([name(), ...]) -> ballot().
make(CandidateNames) ->
    Candidates = lists:map(fun candidate:make/1, CandidateNames),
    #ballot{candidates = Candidates}.

-spec candidates(ballot()) -> [candidate(), ...].
candidates(#ballot{candidates = Candidates}) -> Candidates.

-spec get_2nd_choice_name(ballot()) -> name().
get_2nd_choice_name(Ballot) ->
    [ _, C2 | _ ] = candidates(Ballot),
    candidate:name(C2).

-spec has_top_choice(name(), ballot()) -> boolean().
has_top_choice(Name, Ballot) ->
   Names = lists:map(fun candidate:name/1, candidates(Ballot)),
   hd(Names) =:= Name.

-spec only_multis([ballot()]) -> [ballot()].
only_multis(Ballots) ->
    lists:filter(fun(B) -> length(candidates(B)) > 1 end, Ballots).

%%====================================================================
%% Internal functions
%%====================================================================

