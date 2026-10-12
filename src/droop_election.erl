-module(droop_election).

%% API exports
-export([
    winners/2
]).

-include("include/elections.hrl").

%%====================================================================
%% API functions
%%====================================================================

%% Single transferable vote using the Droop quota. Ballots are transferred
%% whole: on election only the surplus ballots move to their next preference,
%% and when nobody reaches the quota the lowest candidate is eliminated and all
%% of their ballots transfer onward.
-spec winners(pos_integer(), [ballot(), ...]) -> [{candidate_name(), pos_integer()}, ...].
winners(SeatsCount, Ballots) ->
    winners(SeatsCount, [ranking(B) || B <- Ballots], []).

%%====================================================================
%% Internal functions
%%====================================================================

ranking(Ballot) ->
    [ candidate:name(C) || C <- ballot:candidates(Ballot) ].

winners(SeatsCount, _Rankings, Winners) when length(Winners) =:= SeatsCount ->
    lists:reverse(Winners);

winners(SeatsCount, Rankings, Winners) ->
    Quota  = length(Rankings) div (SeatsCount + 1) + 1,
    Counts = top_vote_counts(Rankings),
    case winner(Counts, Quota) of
        {Winner, Votes} ->
            Surplus = Votes - Quota,
            Next = transfer_surplus(Winner, Surplus, Rankings),
            winners(SeatsCount, Next, [{Winner, Votes} | Winners]);
        none ->
            Loser = loser(Counts),
            winners(SeatsCount, eliminate(Loser, Rankings), Winners)
    end.

top_vote_counts(Rankings) ->
    Names = lists:usort([top_name(R) || R <- Rankings]),
    [ {N, length([R || R <- Rankings, top_name(R) =:= N])} || N <- Names ].

top_name([Name | _]) -> Name.

winner(Counts, Quota) ->
    case [ C || {_, V} = C <- counts_desc(Counts), V >= Quota ] of
        []      -> none;
        [W | _] -> W
    end.

loser(Counts) ->
    [{Name, _} | _] = counts_asc(Counts),
    Name.

transfer_surplus(Winner, Surplus, Rankings) ->
    {WinnerTop, Others} = lists:partition(fun(R) -> top_name(R) =:= Winner end, Rankings),
    Transferred = [ T || R <- WinnerTop, T <- [remove(Winner, R)], T =/= [] ],
    Kept = lists:sublist(Transferred, Surplus),
    strip(Winner, Others) ++ Kept.

eliminate(Loser, Rankings) ->
    strip(Loser, Rankings).

strip(Name, Rankings) ->
    [ R2 || R <- Rankings, R2 <- [remove(Name, R)], R2 =/= [] ].

remove(Name, Ranking) ->
    [ X || X <- Ranking, X =/= Name ].

by_votes_desc({N1, V1}, {N2, V2}) ->
    V1 > V2 orelse (V1 =:= V2 andalso N1 =< N2).

by_votes_asc({N1, V1}, {N2, V2}) ->
    V1 < V2 orelse (V1 =:= V2 andalso N1 =< N2).

counts_desc(Counts) -> lists:sort(fun by_votes_desc/2, Counts).

counts_asc(Counts) -> lists:sort(fun by_votes_asc/2, Counts).
