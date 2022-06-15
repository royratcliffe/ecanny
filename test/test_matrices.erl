-module(test_matrices).

-include_lib("eunit/include/eunit.hrl").

transpose_test() ->
    ?assertEqual([], matrices:transpose([])),
    ?assertEqual([[1], [2], [3]], matrices:transpose([[1, 2, 3]])),
    ?assertEqual([[1, 2, 3]], matrices:transpose([[1], [2], [3]])),
    ?assertError(badarg, matrices:transpose([[]])).
