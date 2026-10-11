-module('floyd_warshall_test').
-author('Kevin C. Baird').

-include_lib("eunit/include/eunit.hrl").

floyd_warshall_test_() ->
    [
        fun strongest_path_over_three_vertices_case/0,
        fun single_vertex_yields_empty_paths_case/0,
        fun empty_graph_yields_empty_paths_case/0
    ].

strongest_path_over_three_vertices_case() ->
    Expected = #{ a => #{b => 4, c => 4},
                  b => #{a => 5, c => 4},
                  c => #{a => 5, b => 7} },
    ?assertEqual(Expected, floyd_warshall:widest_paths(three_vertex_graph())).

single_vertex_yields_empty_paths_case() ->
    ?assertEqual(#{}, floyd_warshall:widest_paths(#{ a => #{} })).

empty_graph_yields_empty_paths_case() ->
    ?assertEqual(#{}, floyd_warshall:widest_paths(#{})).

% An incomplete inner map means a zero-strength edge in the missing direction.
three_vertex_graph() ->
    #{ a => #{b => 5, c => 2},
       b => #{a => 3, c => 7},
       c => #{a => 4, b => 6} }.