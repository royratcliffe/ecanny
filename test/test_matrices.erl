-module(test_matrices).

-include_lib("eunit/include/eunit.hrl").

transpose_test() ->
    ?assertEqual([], matrices:transpose([])),
    ?assertEqual([[1], [2], [3]], matrices:transpose([[1, 2, 3]])),
    ?assertEqual([[1, 2, 3]], matrices:transpose([[1], [2], [3]])),
    ?assertError(badarg, matrices:transpose([[]])).

dimensions_test() ->
    ?assertEqual({0, 0}, matrices:dimensions([])),
    ?assertEqual({1, 0}, matrices:dimensions([[]])),
    ?assertEqual({3, 1}, matrices:dimensions([[a], [b], [c]])),
    ?assertEqual({1, 3}, matrices:dimensions([[a, b, c]])).
