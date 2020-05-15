-module(recursion).

-export([fibonacci/1]).


-spec fibonacci(N :: non_neg_integer()) -> pos_integer().
fibonacci(1) ->
    1;
fibonacci(2) ->
    2;
fibonacci(N) when N > 2 ->
    fibonacci(N - 2) + fibonacci(N - 1).

