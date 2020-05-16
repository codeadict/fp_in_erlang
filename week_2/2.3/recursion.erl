-module(recursion).

-include_lib("eunit/include/eunit.hrl").

-export([fibonacci/1,
	 pieces_n_dimension/2,
	 pieces_2d/1,
	 pieces_3d/1,
	 test/0
	]).

%% Calculates Fibonacci numbers
%%
%% References:
%%
%% * https://www.mathsisfun.com/numbers/fibonacci-sequence.html
%%
%% Manual calculation:
%% fibonacci(4)
%% = fibonacci(4, 0, 1)
%% = fibonacci(3, 1, 1)
%% = fibonacci(2, 1, 2)
%% = fibonacci(1, 2, 3)
%% = 3
-spec fibonacci(N :: non_neg_integer()) -> pos_integer().
fibonacci(N) ->
    fibonacci(N, 0, 1).

fibonacci(0, A, _B) ->
    A;
fibonacci(1, _A, B) ->
    B;
fibonacci(N, A, B) ->
    fibonacci(N - 1, B, A + B).

%% Tests for fibonacci/1.
test_fibonacci() ->
    %% Fibonacci sequence from 0 to 20. Taken from https://www.planetmath.org/listoffibonaccinumbers
    FibonacciSeq = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765],
    NumFibMapping = lists:zip(lists:seq(0, length(FibonacciSeq) - 1), FibonacciSeq),
    [?assertEqual(Expected, fibonacci(N), io:format("Failed on N=~p", [N])) || {N, Expected} <- NumFibMapping],
    pass.

%% Determine the maximum number of pieces created by a given
%% number of slices, applied to a space of a given dimension.
%%
%% References:
%%
%% * Robert Banks
%%   Slicing Pizzas, Racing Turtles, and Further Adventures in Applied Mathematics
%%   Princeton, 1999, ISBN13: 9780691059471
%%
%% * https://mathworld.wolfram.com/PlaneDivisionbyLines.html
%% * https://mathworld.wolfram.com/SpaceDivisionbyPlanes.html
%% * https://en.wikipedia.org/wiki/Lazy_caterer%27s_sequence
%%
%% Manual calculation:
%% pieces_n_dimension(3, 2)
%% = pieces_n_dimension(2, 1) + pieces_n_dimension(2, 2)
%% = (pieces_n_dimension(1, 0) + (pieces_n_dimension(1, 2)) + (pieces_n_dimension(1, 1) + pieces_n_dimension(1, 2))
%% = (1 + 2) + (2 + 2)
%% = 3 + 4
%% = 7
-spec pieces_n_dimension(Cuts :: non_neg_integer(), Dimension :: non_neg_integer()) -> pos_integer().
pieces_n_dimension(_Cuts, 0) -> %% when dimension reaches 0 return one piece.
    1;
pieces_n_dimension(0, _Dimension) ->  %% regardless of dimension, 0 cuts will always be one piece.
    1;
pieces_n_dimension(1, _Dimension) -> %% one cut will always return 2 pieces
    2;
pieces_n_dimension(Cuts, Dimension) when Cuts > 1 ->
    pieces_n_dimension(Cuts - 1, Dimension - 1) + pieces_n_dimension(Cuts - 1, Dimension).

%% Shortcut function for max number of pieces in a plane (2 dimensional).
-spec pieces_2d(Cuts :: non_neg_integer()) -> pos_integer().
pieces_2d(Cuts) ->
    pieces_n_dimension(Cuts, 2).

%% Shortcut function for max number of pieces in a 3 dimensional space.
-spec pieces_3d(Cuts :: non_neg_integer()) -> pos_integer().
pieces_3d(Cuts) ->
    pieces_n_dimension(Cuts, 3).

%% Tests for pieces_2d/1.
test_pieces_2d() ->
    %% Lazy caterer sequence for 0 to 20. Taken from: https://en.wikipedia.org/wiki/Lazy_caterer%27s_sequence
    CatererSeq = [1, 2, 4, 7, 11, 16, 22, 29, 37, 46, 56, 67, 79, 92, 106, 121, 137, 154, 172, 191, 211],
    NumCatererSeqMapping = lists:zip(lists:seq(0, length(CatererSeq) - 1), CatererSeq),
    [?assertEqual(Expected, pieces_2d(N), io:format("Failed on N=~p", [N])) || {N, Expected} <- NumCatererSeqMapping],
    pass.

%% Tests for pieces_3d/1.
test_pieces_3d() ->
    %% Cake numbers sequence from 0 to 20. Taken from: https://en.wikipedia.org/wiki/Cake_number and https://oeis.org/A000125.
    CakeSeq = [1, 2, 4, 8, 15, 26, 42, 64, 93, 130, 176, 232, 299, 378, 470, 576, 697, 834, 988, 1160, 1351],
    NumCakeSeqMapping = lists:zip(lists:seq(0, length(CakeSeq) - 1), CakeSeq),
    [?assertEqual(Expected, pieces_3d(N), io:format("Failed on N=~p", [N])) || {N, Expected} <- NumCakeSeqMapping],
    pass.

%% General tests.
%% TODO: test more than 3 dimensions.
test() ->
    pass = test_fibonacci(),
    pass = test_pieces_2d(),
    pass = test_pieces_3d().
