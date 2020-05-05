-module(first).

-export([mult/2, double/1, treble/1, square/1, area/3]).

%% Multiply two numbers.
-spec mult(X :: number(), Y :: number()) -> number().
mult(X, Y) ->
    X * Y.

%% Duplicates a number.
-spec double(X :: number()) -> number().
double(X) -> 
    mult(2, X).

%% Triple in a number.
-spec treble(X :: number()) -> number().
treble(X) -> 
    mult(3, X).

%% Square a number: multiply it by itself.
-spec square(X :: number()) -> number().
square(X) ->
    mult(X, X).

%% Calculates the area of a triangle.
-spec area(X :: number(), Y :: number(), Z :: number()) -> number().
area(X, Y, Z) ->
    S = (X + Y + Z) / 2,
    math:sqrt(S * (S - X) * (S - Y) * (S - Z)).
