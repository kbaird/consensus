-module(candidate).

%% API exports
-export([
    make/1,
    name/1,
    party/1,
    rank/1
]).

-include("include/elections.hrl").
-include("include/parties.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec make(name()) -> candidate().
make({Name, Party}) when is_atom(Name) andalso is_atom(Party) ->
    #candidate{name = Name, party = Party};
make(Name) when is_atom(Name) -> #candidate{name = Name}.

-spec name(candidate()) -> name().
name(#candidate{name = Name}) -> Name.

-spec party(candidate()) -> party_name().
party(#candidate{party = Party}) -> Party.

-spec rank(preferences()) -> [candidate(), ...].
rank(Prefs) ->
    case length(maps:to_list(Prefs)) of
        1 -> [ C || {C, _} <- maps:to_list(Prefs) ];
        _ -> schulze_rank(Prefs)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

% https://wiki.electorama.com/wiki/Schulze_method#Implementation
-spec schulze_rank(preferences()) -> [candidate(), ...].
schulze_rank(Prefs) ->
    Candidates = maps:keys(Prefs),
    TwoCs   = [ {I, J} || I <- Candidates,
                          J <- Candidates,
                          I =/= J ],
    Pass1   = floyd_warshall_stage1(Prefs, maps:new(), TwoCs),
    FullCs  = [ {I, J, K} || I <- Candidates,
                             J <- Candidates,
                             K <- Candidates,
                             I =/= J,
                             I =/= K,
                             J =/= K ],
    Set = floyd_warshall_stage2(Pass1, FullCs),
    Lst = maps:to_list(Set),
    Sorted = lists:sort(fun schulze_sorter/2, Lst),
    [ Candidate || {Candidate, _} <- Sorted ].

schulze_sorter({_, M1}, {_, M2}) ->
    lists:sum(maps:values(M1)) <
    lists:sum(maps:values(M2)).

floyd_warshall_stage1(_D, P, [])                -> P;
floyd_warshall_stage1(D, P0, [ {I, J} | Rest ]) ->
    IVal   = maps:get(I, D),
    JOverI = maps:get(J, IVal, 0),
    JVal   = maps:get(J, D),
    IOverJ = maps:get(I, JVal, 0),
    V = case IOverJ > JOverI of
            true -> IOverJ;
            _    -> 0
        end,
    PI = maps:get(I, P0, maps:new()),
    PJ = maps:put(J, V, PI),
    P  = maps:put(I, PJ, P0),
    floyd_warshall_stage1(D, P, Rest).

floyd_warshall_stage2(P,  []) -> P;
floyd_warshall_stage2(P0, [ {I, J, K} | Rest ]) ->
    PI   = maps:get(I, P0),
    PJ0  = maps:get(J, P0),
    PJI0 = maps:get(I, PJ0),
    PJK0 = maps:get(K, PJ0),
    PIK0 = maps:get(K, PI),
    PJK  = lists:max([ PJK0, lists:min([PJI0, PIK0]) ]),
    PJ   = maps:put(K, PJK, PJ0),
    P    = maps:put(J, PJ, P0),
    floyd_warshall_stage2(P, Rest).
