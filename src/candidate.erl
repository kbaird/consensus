-module(candidate).

%% API exports
-export([
    make/1,
    is_candidate/1,
    name/1,
    party/1,
    rank/1
]).

-include("include/elections.hrl").
-include("include/parties.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec make(candidate_name()) -> candidate().
make({Name, Party}) when is_atom(Name) andalso is_binary(Party) ->
    #candidate{name = Name, party = Party};
make(Name) when is_atom(Name) -> #candidate{name = Name}.

-spec is_candidate(term()) -> boolean().
is_candidate(#candidate{}) -> true;
is_candidate(_Other)       -> false.

-spec name(candidate()) -> candidate_name().
name(#candidate{name = Name}) -> Name.

-spec party(candidate()) -> party_name().
party(#candidate{party = Party}) -> Party.

-spec rank(preferences()) -> [candidate(), ...].
rank(Prefs) ->
    case maps:size(Prefs) of
        1 -> maps:keys(Prefs);
        _ -> schulze_rank(Prefs)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

% Cf. https://wiki.electorama.com/wiki/Schulze_method#Implementation
-spec schulze_rank(preferences()) -> [candidate(), ...].
schulze_rank(Prefs) ->
    PathStrengths = floyd_warshall:widest_paths(Prefs),
    Lst           = maps:to_list(PathStrengths),
    Sorted        = lists:sort(fun schulze_sorter/2, Lst),
    [ Candidate || {Candidate, _} <- Sorted ].

schulze_sorter({_, M1}, {_, M2}) ->
    lists:sum(maps:values(M1)) <
    lists:sum(maps:values(M2)).
