-module(recursion).

-export([fibonacci/1, pieces/2]).


%% Calculates Fibonacci numbers
%%
%% References:
%%
%% https://www.mathsisfun.com/numbers/fibonacci-sequence.html
%%
%% Manual calculation for fibonacci(4)
%% = fibonacci(2) + fibonacci(3)
%% = 1 + (fibonacci(1) + fibonacci(2))
%% = 1 + (1 + 1)
%% = 3
-spec fibonacci(N :: non_neg_integer()) -> pos_integer().
fibonacci(0) ->
    0;
fibonacci(1) ->
    1;
fibonacci(2) ->
    1;
fibonacci(N) when N > 2 ->
    fibonacci(N - 2) + fibonacci(N - 1).


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
pieces(0, _) ->  %% regardless of dimension, 0 cuts will always be one piece.
    1;
pieces(1, _) -> %% one cut will always return 2 pieces
    2;
pieces(Cuts, Dimension) when Cuts > 1 ->
    pieces(Cuts - 1, Dimension - 1) + pieces(Cuts - 1, Dimension).


