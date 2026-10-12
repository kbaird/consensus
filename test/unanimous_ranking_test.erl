-module('unanimous_ranking_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

% Every voter submits the same ranking; the Condorcet winner is its first
% choice and the reported rankings echo the ballot, for any candidate count.
unanimous_ranking_test_() ->
    [ {unanimous_ranking_description(Ranking),
       fun() -> assert_unanimous_ranking(Ranking) end}
      || Ranking <- unanimous_rankings() ].

unanimous_rankings() ->
    [ [a], [a, b], [a, b, c] ].

%%% PRIVATE FUNCTIONS

assert_unanimous_ranking(Ranking) ->
    Ballots  = lists:map(fun ballot:make/1, lists:duplicate(4, Ranking)),
    Winner   = consensus:condorcet_winner(Ballots),
    ?assertEqual(hd(Ranking), Winner),
    Rankings = consensus:condorcet_rankings(Ballots),
    ?assertEqual(Ranking, Rankings).

unanimous_ranking_description(Ranking) ->
    lists:flatten(io_lib:format("unanimous ranking ~p", [Ranking])).