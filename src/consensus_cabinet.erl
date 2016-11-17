-module(consensus_cabinet).

%% API exports
-export([
    compose/2
]).

-include("parties.hrl").

%%====================================================================
%% API functions
%%====================================================================

% Cf. Lijphart, Arend, _Patterns of Democracy_, 1999. pg93.
-spec compose(atom(), [{party_name(), seat_share()}, ...]) ->
    [party_name(), ...].
compose(bargaining_proposition, SeatShares) -> compose(bp, SeatShares);
compose(bp, SeatShares) ->
    JustParties = compose(mwc, SeatShares),
    NumParties  = [ length(Ps) || Ps <- JustParties ],
    Min         = lists:min(NumParties),
    [ Cab || Cab <- JustParties, length(Cab) =:= Min ];


compose(minimal_connected_winning, SeatShares) -> compose(mcw, SeatShares);
compose(minimum_connected_winning, SeatShares) -> compose(mcw, SeatShares);
compose(mcw, _SeatShares) ->
    [];


compose(minimal_range, SeatShares) -> compose(mr, SeatShares);
compose(minimum_range, SeatShares) -> compose(mr, SeatShares);
compose(mr, _SeatShares) ->
    [];


compose(minimal_size, SeatShares) -> compose(ms, SeatShares);
compose(minimum_size, SeatShares) -> compose(ms, SeatShares);
compose(ms, SeatShares) ->
    Cabs      = mwc_with_seats(SeatShares),
    Shares    = [ share(C) || C <- Cabs ],
    MinSize   = lists:min(Shares),
    WithSeats = [ Cab || Cab <- Cabs, share(Cab) =:= MinSize ],
    just_party_names(WithSeats);


compose(minimal_winning_coalition, SeatShares) -> compose(mwc, SeatShares);
compose(minimum_winning_coalition, SeatShares) -> compose(mwc, SeatShares);
compose(mwc, SeatShares) ->
    WithSeats = mwc_with_seats(SeatShares),
    just_party_names(WithSeats);

compose(policy_viable_coalition, SeatShares) -> compose(pvc, SeatShares);
compose(pvc, _SeatShares) ->
    [].

%%====================================================================
%% Internal functions
%%====================================================================
is_coalition([{_,_}]) -> false;
is_coalition(_)       -> true.

is_winner(Coalition, SeatShares) ->
    share(Coalition) > share(SeatShares) / 2.0.

just_party_names(Ls) ->
    [ lists:map(fun({Name, _Cnt}) -> Name end, L) || L <- Ls ].

mwc_with_seats(SeatShares) ->
    PSet       	= powerset(SeatShares),
    Unique      = uniqueify(PSet),
    Coalitions  = lists:filter(fun is_coalition/1, Unique),
    Winners     = lists:filter(fun(C) -> is_winner(C, SeatShares) end, Coalitions),
    lists:filter(fun(C) -> not too_large(C, Winners) end, Winners).

powerset([]) -> [[]];
powerset([H|T]) ->
    PT = powerset(T),
    powerset(H, PT, PT).

powerset(_, [],    Acc) -> Acc;
powerset(X, [H|T], Acc) ->
    powerset(X, T, [[X|H]|Acc]).

share({_Name, Cnt}) -> Cnt;
share(L) ->
    lists:foldl(fun({_N, Cnt}, Sum) -> Cnt + Sum end, 0, L).

smallers(C, Coalitions) ->
    [ Coalition ||  Coalition  <- Coalitions,
                    length(Coalition) < length(C),
                    lists:all(fun(Party) -> lists:member(Party, C) end, Coalition) ].

too_large(C, Coalitions) ->
    Smallers = smallers(C, Coalitions),
    length(Smallers) > 0.

uniqueify(Ls) ->
    lists:usort([ lists:usort(L) || L <- Ls ]).
