-module(droop_election).

%% API exports
-export([
    winners/2
]).

-include("elections.hrl").

%%====================================================================
%% API functions
%%====================================================================

-spec winners(pos_integer(), [ballot(), ...]) -> [{name(), pos_integer()}, ...].
winners(SeatsCount, Ballots) -> droop_winners(SeatsCount, Ballots, []).

%%====================================================================
%% Internal functions
%%====================================================================

droop_winners(SeatsCount, _Bs, Winners) when length(Winners) =:= SeatsCount ->
    lists:reverse(Winners);
droop_winners(SeatsCount, Ballots, Winners) ->
    Quota = length(Ballots) div (SeatsCount+1) + 1,
    NestedCands = [ ballot:candidates(B) || B <- Ballots ],
    CNamesWithVotes = candidate_names_with_votes(NestedCands),
    [ Winner | _ ] = lists:sort(fun({_, V1}, {_, V2}) -> V1 > V2 end, CNamesWithVotes),
    droop_winners(SeatsCount, ballots_without(Ballots, Winner, Quota, CNamesWithVotes), [ Winner | Winners ]).

ballots_without(Ballots, {WinnerName, WinnerVotes}, Quota, [ _, _NextCandidate | _ ]) ->
    Raw   = ballots_without(Ballots, {WinnerName, WinnerVotes}, Quota, []),
    Names = [ NextName || {ballot, [{candidate, FirstName}, {candidate, NextName} | _]} <- Ballots,
                          FirstName =:= WinnerName ],
    VoteCountToTransfer = WinnerVotes - Quota,
    Raw ++ [ ballot:make([NextName]) || NextName <- lists:sublist(Names, VoteCountToTransfer) ];
ballots_without(Ballots, {WinnerName, _WinnerVotes}, _Quota, _) ->
    [ B || B <- Ballots, not lists:member({candidate, WinnerName}, ballot:candidates(B)) ].

candidate_names_with_votes(NestedCs) ->
    CNames = [ candidate:name(C) || C <- lists:usort(lists:flatten(NestedCs)) ],
    [ {N, length(lists:filter(fun([{candidate, Name} | _]) -> Name =:= N end, NestedCs))} || N <- CNames ].

