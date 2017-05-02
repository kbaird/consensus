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
    schulze_rank(Prefs).

%%====================================================================
%% Internal functions
%%====================================================================
-spec by_least_votes(preferences()) ->
    fun((candidate(), candidate()) -> boolean()).
by_least_votes(Prefs) ->
    % I think this may actually be closer to
    % https://en.wikipedia.org/wiki/Ranked_pairs than Schulze
    % (both satisfy Condorcet criteria)
    fun(C1, C2) ->
        maps:get(C1, maps:get(C2, Prefs), 0) <
        maps:get(C2, maps:get(C1, Prefs), 0)
    end.

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
    [ Winner || Winner <- maps:keys(Set),
                Loser  <- maps:get(Winner, Set),
                Value  <- maps:values(Winner),
                Value == 0 ].

floyd_warshall_stage1(_D, P, []) -> P;
floyd_warshall_stage1(D, P0, [ {I, J} | Rest ]) ->
    JOverI = maps:get(J, maps:get(I, D)),
    IOverJ = maps:get(I, maps:get(J, D)),
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
    JOverK = maps:get(J, maps:get(K, P0)),
    IOverJ = maps:get(I, maps:get(J, P0)),
    PI   = maps:get(I, P0),
    PJ0  = maps:get(J, P0),
    PJI0 = maps:get(I, PJ0),
    PJK0 = maps:get(K, PJ0),
    PIK0 = maps:get(K, PI),
    PJK  = lists:max([ PJK0, lists:min([PJI0, PIK0]) ]),
    PJ   = maps:put(K, PJK, PJ0),
    P    = maps:put(J, PJ, P0),
    floyd_warshall_stage2(P, Rest).
