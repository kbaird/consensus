-module(droop_election).

%% API exports
-export([
    winners/2
]).

-include("condorcet.hrl").

%%====================================================================
%% API functions
%%====================================================================

-spec winners(pos_integer(), [ballot(), ...]) -> [name(), ...].
winners(SeatsCount, Ballots) -> droop_winners(SeatsCount, Ballots, []).

%%====================================================================
%% Internal functions
%%====================================================================

droop_winners(SeatsCount, _Bs, Winners) when length(Winners) =:= SeatsCount ->
    lists:reverse(Winners);
droop_winners(SeatsCount, Ballots, Winners) ->
    %Quota = length(Ballots) / (SeatsCount+1) + 1,
    NestedCands = [ condorcet_ballot:candidates(B) || B <- Ballots ],
    CNamesWithVotes = sorted_candidate_names_with_votes(NestedCands),
    [ {Winner, _Votes} | _ ] = lists:sort(fun({_, V1}, {_, V2}) -> V1 > V2 end,
                                          CNamesWithVotes),
    % TODO: Don't give 2nd place candidate all of winner's votes in next round, just the surplus beyond the quota
    droop_winners(SeatsCount, ballots_without(Ballots, Winner), [ Winner | Winners ]).

ballots_without(Ballots, Winner) ->
    CandsWithoutWinner  = fun({candidate, CN}) -> CN =/= Winner end,
    BallotWithoutWinner = fun(B) -> {ballot, lists:filter(CandsWithoutWinner,
                                                          condorcet_ballot:candidates(B))}
                          end,
    lists:map(BallotWithoutWinner, Ballots).

sorted_candidate_names_with_votes(NestedCands) ->
    CNames = [ condorcet_candidate:name(C) ||
               C <- lists:usort(lists:flatten(NestedCands)) ],
    [ {N, length(lists:filter(fun([{candidate, Name} | _]) -> Name =:= N end, NestedCands))} || N <- CNames ].

