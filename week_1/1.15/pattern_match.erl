-module(pattern_match).

-export([xor1/2, xor2/2, xor3/2, how_many_equal/3, max_of_three/3]).
-export([test/0]).

%% ------------------------------------------------------------------
%% In the previous video step on pattern matching we saw two ways of 
%% defining “exclusive or”. Give at least three others.
%% ------------------------------------------------------------------

xor1(X, Y) ->
    X =/= Y.

xor2(X, Y) ->
    not X == Y.

xor3(X, Y) ->
    X and not Y or Y and not X.

%% Text for xor implementations.
test_xor() ->
    %% Test xor1/2.
    true = xor1(true, false),
    true = xor1(false, true),
    false = xor1(true, true),
    false = xor1(false, false),
    %% Test xor2/2.
    true = xor2(true, false),
    true = xor2(false, true),
    false = xor2(true, true),
    false = xor2(false, false),
    %% Test xor3/2.
    true = xor3(true, false),
    true = xor3(false, true),
    false = xor3(true, true),
    false = xor3(false, false),
    pass.


%% ----------------------------------------------------------------
%% Give a definition of the function howManyEqual which takes three
%% integers and returns an integer, counting how many of its three 
%% arguments are equal
%% ----------------------------------------------------------------

how_many_equal(_X, _X, _X) ->
    3;
how_many_equal(_X, _X, _Y) ->
    2;
how_many_equal(_X, _Y, _X) ->
    2;
how_many_equal(_Y, _X, _X) ->
    2;
how_many_equal(_, _, _) ->
    0.

%% Tests for how_many_equal.
test_how_many_equal() ->
    0 = how_many_equal(34, 25, 36),
    2 = how_many_equal(34, 25, 34),
    3 = how_many_equal(34, 34, 34),
    pass.


%% ---------------------------------------------------------------------
%% Give a definition of the function maxThree which takes three integers
%% and returns the maximum of the three. You can use the max function,
%% which gives the maximum of two numbers, in writing your definition.
%% --------------------------------------------------------------------

max_of_three(X, Y, Z) ->
    max(max(X, Y), Z).

%% Tests for max_of_three.
test_max_three() ->
    2 = max_of_three(0, 1, 2),
    15 = max_of_three(-1000, 5, 15),
    4 = max_of_three(2, 0.01, 2#100),
    pass.


%% -----------------------------------------------------
%% General test to ensure all the individual tests pass.
%% -----------------------------------------------------

test() ->
    pass = test_xor(),
    pass = test_max_three(),
    pass = test_how_many_equal().
