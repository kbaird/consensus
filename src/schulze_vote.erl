-module(schulze_vote).

%% API exports
-export([
    make_ballot/1,
    winner/1
]).

-include("schulze.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec make_ballot([candidate(), ...]) -> ballot().
make_ballot(Ballot) -> Ballot.

-spec winner([ballot(), ...]) -> candidate().
winner(Ballots) -> winner(Ballots, #{}).

%%====================================================================
%% Internal functions
%%====================================================================
winner([Ballot | Bs], Acc) ->
    Candidate = hd(Ballot),
    Count     = maps:get(Candidate, Acc, 0),
    winner(Bs, maps:put(Candidate, Count+1, Acc));
winner([],            Acc) ->
    L = maps:to_list(Acc),
    Most = fun({X, Xcnt}, {Y, Ycnt}) -> Xcnt >= Ycnt end,
    {Candidate, Count} = hd(lists:sort(Most, L)),
    Candidate.
