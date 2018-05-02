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
    Quota       = length(Ballots) / (SeatsCount+1) + 1,
    NestedCands = [ ballot:candidates(B) || B <- Ballots ],
    CNamesWithVotes = candidate_names_with_top_votes(NestedCands),
    [ Winner | _ ]  = lists:sort(fun({_, V1}, {_, V2}) -> V1 > V2 end, CNamesWithVotes),
    Ballots2 = ballots_without(Ballots, Winner, Quota, CNamesWithVotes),
    droop_winners(SeatsCount, Ballots2, [ Winner | Winners ]).

ballots_without(Ballots, {WinnerName, WinnerVotes}, Quota, [ _, _NextCandidate | _ ]) ->
    Raw       = ballots_without(Ballots, {WinnerName, WinnerVotes}, Quota, []),
    NextNames = tail_names_from(WinnerName, Ballots),
    More      = case WinnerVotes > Quota of
                    true -> lists:sublist(NextNames, WinnerVotes - trunc(Quota));
                    _    -> NextNames
                end,
    Raw ++ [ ballot:make([NextName]) || NextName <- lists:flatten(More) ];
ballots_without(Ballots, {WinnerName, _WinnerVotes}, _Quota, _) ->
    [ B || B <- Ballots,
           not lists:member(WinnerName, lists:map(fun candidate:name/1, ballot:candidates(B))) ].

candidate_names_with_top_votes(NestedCs) ->
    CNames = [ candidate:name(C) || C <- lists:usort(lists:flatten(NestedCs)) ],
    [ {N, length(lists:filter(fun([C|_]) -> candidate:name(C) =:= N end, NestedCs))} || N <- CNames ].

tail_names_from(WinnerName, Ballots) ->
    [ ballot:get_tail_names(B) || B <- ballot:only_multis(Ballots),
                                  ballot:has_top_choice(WinnerName, B) ].
