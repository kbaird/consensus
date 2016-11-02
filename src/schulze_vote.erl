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
winner(Ballots) ->
    {Candidate, _} = winner(Ballots, #{}),
    Candidate.

%%====================================================================
%% Internal functions
%%====================================================================
most({_, Xcnt}, {_, Ycnt}) -> Xcnt >= Ycnt.

winner([], Acc) when is_map(Acc) -> winner([], maps:to_list(Acc));
winner([], Acc)                  -> hd(lists:sort(fun most/2, Acc));
winner([Ballot | Bs], Acc) ->
    Candidate = hd(Ballot),
    Count     = maps:get(Candidate, Acc, 0),
    winner(Bs, maps:put(Candidate, Count+1, Acc)).
