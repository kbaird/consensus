-module(consensus_utils).

%% API exports
-export([
    binary_to_char/1,
    powerset/1,
    uniqueify/1
]).

-include("include/common.hrl").
-include("include/parties.hrl").

%%====================================================================
%% API functions
%%====================================================================

-spec binary_to_char(binary()) -> char().
binary_to_char(Bin) -> hd(binary_to_list(Bin)).

-spec powerset([T]) -> [[T]].
powerset([])    -> [[]];
powerset([H|T]) ->
    PT = powerset(T),
    powerset(H, PT, PT).

-spec uniqueify([[T]]) -> [[T]].
uniqueify(Ls) -> lists:usort([ lists:usort(L) || L <- Ls ]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec powerset(T, [[T]], [[T]]) -> [[T]].
powerset(_, [],    Acc) -> Acc;
powerset(X, [H|T], Acc) -> powerset(X, T, [[X|H]|Acc]).

