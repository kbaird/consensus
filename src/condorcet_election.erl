-module(condorcet_election).

%% API exports
-export([
    droop_winners/2,
    rankings/1,
    winner/1
]).

-include("condorcet.hrl").

%%====================================================================
%% API functions
%%====================================================================

-spec droop_winners(pos_integer(), [ballot(), ...]) -> [name(), ...].
droop_winners(SeatsCount, Ballots) -> droop_winners(SeatsCount, Ballots, []).

droop_winners(SeatsCount, _Bs, Winners) when length(Winners) =:= SeatsCount ->
    lists:reverse(Winners);
droop_winners(SeatsCount, Ballots, Winners) ->
    %Quota = length(Ballots) / (SeatsCount+1) + 1,
    NestedCands = [ condorcet_ballot:candidates(B) || B <- Ballots ],
    CNamesWithVotes = sorted_candidate_names_with_votes(NestedCands),
    [ {Winner, _Votes} | _ ] = lists:sort(fun({_N1, Vs1}, {_N2, Vs2}) -> Vs1 > Vs2 end, CNamesWithVotes),
    droop_winners(SeatsCount, ballots_without(Ballots, Winner), [ Winner | Winners ]).

ballots_without(Ballots, Winner) ->
    CandsWithoutWinner  = fun({candidate, CN}) -> CN =/= Winner end,
    BallotWithoutWinner = fun(B) -> {ballot, lists:filter(CandsWithoutWinner, condorcet_ballot:candidates(B))} end,
    lists:map(BallotWithoutWinner, Ballots).

sorted_candidate_names_with_votes(NestedCands) ->
    CNames = [ condorcet_candidate:name(C) || C <- lists:usort(lists:flatten(NestedCands)) ],
    [ {N, length(lists:filter(fun([{candidate, Name} | _]) -> Name =:= N end, NestedCands))} || N <- CNames ].

-spec rankings([ballot(), ...]) -> [name(), ...].
rankings(Ballots) ->
    Prefs  = preferences(Ballots),
    Ranked = condorcet_candidate:rank(Prefs),
    [ condorcet_candidate:name(C) || C <- Ranked ].

-spec winner([ballot(), ...]) -> name().
winner(Ballots) -> hd(rankings(Ballots)).

%%====================================================================
%% Internal functions
%%====================================================================
-spec add_preferences(candidate(),
                      [candidate(), ...],
                      map()) -> preferences().
add_preferences(_LastPlaceCand, [],     Acc)   -> Acc;
add_preferences(Cand,  [ Next | Rest ], AccIn) ->
    Acc1 = increment_vote_count(Cand, Next, AccIn),
    Acc2 = add_preferences(Cand, Rest, Acc1),
    add_preferences(Next, Rest, Acc2).

-spec increment_vote_count(candidate(),
                           candidate(),
                           preferences()) -> preferences().
increment_vote_count(Cand, Next, PrefsIn) ->
    WithCount   = maps:get(Cand, PrefsIn, maps:new()),
    Count       = maps:get(Next, WithCount, 0),
    Incremented = maps:put(Next, Count+1, WithCount),
    maps:put(Cand, Incremented, PrefsIn).

-spec preferences(list()) -> preferences().
preferences(Ballots) -> preferences(Ballots, maps:new()).

-spec preferences(list(), map()) -> preferences().
preferences([],            Acc)   -> Acc;
preferences([Ballot | Bs], AccIn) ->
    [ Cand | Rest ] = condorcet_ballot:candidates(Ballot),
    case Rest of
        [] -> maps:put(Cand, winner, maps:new());
        _  -> Acc = add_preferences(Cand, Rest, AccIn),
              preferences(Bs, Acc)
    end.
