-module(floyd_warshall).

%% API exports
-export([
    widest_paths/1
]).

%%====================================================================
%% API functions
%%====================================================================

% Computes the strongest path for every ordered pair of vertices: the
% strength of a path is the strength of its weakest edge, and the result for
% a pair is the maximum strength over all paths, per the Schulze method.
% Cf. https://wiki.electorama.com/wiki/Schulze_method#Implementation
-spec widest_paths(map()) -> map().
widest_paths(Weights) ->
    Vertices = maps:keys(Weights),
    OneHops  = [ {I, J} || I <- Vertices, J <- Vertices, I =/= J ],
    Initial  = one_hop_strengths(Weights, maps:new(), OneHops),
    Triples  = [ {I, J, K} || I <- Vertices,
                              J <- Vertices,
                              K <- Vertices,
                              I =/= J,
                              I =/= K,
                              J =/= K ],
    strengthen_paths(Initial, Triples).

%%====================================================================
%% Internal functions
%%====================================================================

one_hop_strengths(_Weights, Strengths, []) -> Strengths;
one_hop_strengths(Weights, Strengths0, [ {From, To} | Rest ]) ->
    Forward   = maps:get(To, maps:get(From, Weights), 0),
    Backward  = maps:get(From, maps:get(To, Weights), 0),
    Strength  = case Backward > Forward of
                    true  -> Backward;
                    false -> 0
                end,
    HopMap    = maps:get(From, Strengths0, maps:new()),
    WithHop   = maps:put(To, Strength, HopMap),
    Strengths = maps:put(From, WithHop, Strengths0),
    one_hop_strengths(Weights, Strengths, Rest).

strengthen_paths(Strengths, []) -> Strengths;
strengthen_paths(Strengths0, [ {I, J, K} | Rest ]) ->
    PI    = maps:get(I, Strengths0),
    PJ0   = maps:get(J, Strengths0),
    PJI0  = maps:get(I, PJ0),
    PJK0  = maps:get(K, PJ0),
    PIK0  = maps:get(K, PI),
    PIK   = lists:min([PJI0, PIK0]),
    PJK   = lists:max([PJK0, PIK]),
    PJ    = maps:put(K, PJK, PJ0),
    Next  = maps:put(J, PJ, Strengths0),
    strengthen_paths(Next, Rest).