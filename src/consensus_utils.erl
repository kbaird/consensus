-module(consensus_utils).

%% API exports
-export([
    atom_to_char/1,
    powerset/1,
    uniqueify/1
]).

-include("include/common.hrl").
-include("include/parties.hrl").

%%====================================================================
%% API functions
%%====================================================================

-spec atom_to_char(atom()) -> char().
atom_to_char(Atom) -> hd(atom_to_list(Atom)).

powerset([])    -> [[]];
powerset([H|T]) ->
    PT = powerset(T),
    powerset(H, PT, PT).

uniqueify(Ls) -> lists:usort([ lists:usort(L) || L <- Ls ]).

%%====================================================================
%% Internal functions
%%====================================================================

powerset(_, [],    Acc) -> Acc;
powerset(X, [H|T], Acc) -> powerset(X, T, [[X|H]|Acc]).

