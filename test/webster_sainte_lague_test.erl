-module('webster_sainte_lague_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").
-include_lib("elections.hrl").

webster_sainte_lague_setup()     -> ok.
webster_sainte_lague_teardown(_) -> ok.

% https://en.wikipedia.org/wiki/Webster/Sainte-Lagu%C3%AB_method#Description_of_the_method
webster_sainte_lague_test_() ->
    {setup, fun webster_sainte_lague_setup/0,
            fun webster_sainte_lague_teardown/1,
            [
                fun webster_sainte_lague_basic_case/0
            ]
    }.

webster_sainte_lague_basic_case() ->
    Votes    = [ {a, 53000}, {b, 24000}, {c, 23000} ],
    Rankings = consensus:webster_sainte_lague_rankings(Votes, 7),
    ?assertEqual([{a, 3, 3.71}, {b, 2, 1.68}, {c, 2, 1.61}], Rankings).

%%% PRIVATE FUNCTIONS

