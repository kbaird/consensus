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
    % TODO: What if top vote getter is below quota?
    [ Winner | _ ] = lists:sort(fun({_, V1}, {_, V2}) -> V1 > V2 end, CNamesWithVotes),
    Ballots2 = ballots_without(Ballots, Winner, Quota, CNamesWithVotes),
    droop_winners(SeatsCount, Ballots2, [ Winner | Winners ]).

ballots_without(Ballots, {WinnerName, WinnerVotes}, Quota, [ _, _NextCandidate | _ ]) ->
    Raw   = ballots_without(Ballots, {WinnerName, WinnerVotes}, Quota, []),
    % TODO: stop peeking inside ballot and candidate. Use provided funs.
    Names = [ NextName || {ballot, [{candidate, FirstName}, {candidate, NextName} | _]} <- Ballots,
                          FirstName =:= WinnerName ],
    VoteCountToTransfer = WinnerVotes - Quota,
    Raw ++ [ ballot:make([NextName]) || NextName <- lists:sublist(Names, VoteCountToTransfer) ];
ballots_without(Ballots, {WinnerName, _WinnerVotes}, _Quota, _) ->
    % TODO: stop peeking inside ballot and candidate. Use provided funs.
    [ B || B <- Ballots, not lists:member({candidate, WinnerName}, ballot:candidates(B)) ].

candidate_names_with_votes(NestedCs) ->
    CNames = [ candidate:name(C) || C <- lists:usort(lists:flatten(NestedCs)) ],
    % TODO: stop peeking inside ballot and candidate. Use provided funs.
    [ {N, length(lists:filter(fun([{candidate, Name} | _]) -> Name =:= N end, NestedCs))} || N <- CNames ].

