-module(consensus_condorcet).

%% API exports
-export([
    rankings/1,
    winner/1
]).

-include("parties.hrl").
-include("condorcet.hrl").

%%====================================================================
%% API functions
%%====================================================================

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
add_preferences(_Cand, [], Acc) -> Acc;
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
preferences(Ballots) -> preferences(Ballots, #{}).

-spec preferences(list(), map()) -> preferences().
preferences([],            Acc)   -> Acc;
preferences([Ballot | Bs], AccIn) ->
    [ Cand | Rest ] = condorcet_ballot:candidates(Ballot),
    case Rest of
        [] -> maps:put(Cand, winner, maps:new());
        _  -> Acc = add_preferences(Cand, Rest, AccIn),
              preferences(Bs, Acc)
    end.
