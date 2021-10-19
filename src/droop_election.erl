-module(droop_election).

%% API exports
-export([
    winners/2
]).

-include("include/elections.hrl").

%%====================================================================
%% API functions
%%====================================================================

-spec winners(pos_integer(), [ballot(), ...]) -> [{candidate_name(), pos_integer()}, ...].
winners(SeatsCount, Ballots) -> droop_winners(SeatsCount, Ballots, []).

%%====================================================================
%% Internal functions
%%====================================================================

by_votes({_, V1}, {_, V2}) -> V1 > V2.

droop_winners(SeatsCount, _Bs, Winners) when length(Winners) =:= SeatsCount ->
    lists:reverse(Winners);

droop_winners(SeatsCount, Ballots, Winners) ->
    Quota       = length(Ballots) div (SeatsCount+1) + 1,
    NestedCands = [ ballot:candidates(B) || B <- Ballots ],
    CNamesWithVotes = candidate_names_with_top_votes(NestedCands),
    [ Winner | _ ]  = lists:sort(fun by_votes/2, CNamesWithVotes),
    Ballots2 = ballots_without(Ballots, Winner, Quota, CNamesWithVotes),
    droop_winners(SeatsCount, Ballots2, [ Winner | Winners ]).

ballots_without(Ballots, {WinnerName, WinnerVotes}, Quota, [ _, _NextCandidate | _ ]) ->
    Raw   = ballots_without(Ballots, {WinnerName, WinnerVotes}, Quota, []),
    Names = names_from(WinnerName, Ballots),
    More  = case WinnerVotes > Quota of
                true -> lists:sublist(Names, WinnerVotes - Quota);
                _    -> Names
            end,
    Raw ++ [ ballot:make([NextName]) || NextName <- More ];

ballots_without(Ballots, {WinnerName, _WinnerVotes}, _Quota, _) ->
    [ B || B <- Ballots,
           not lists:member(WinnerName, lists:map(fun candidate:name/1, ballot:candidates(B))) ].

candidate_names_with_top_votes(NestedCs) ->
    CNames = [ candidate:name(C) || C <- lists:usort(lists:flatten(NestedCs)) ],
    [ {N, length(lists:filter(fun([C|_]) -> candidate:name(C) =:= N end,
                              NestedCs))} || N <- CNames ].

names_from(WinnerName, Ballots) ->
    [ ballot:get_2nd_choice_name(B) || B <- ballot:only_multis(Ballots),
                                       ballot:has_top_choice(WinnerName, B) ].
