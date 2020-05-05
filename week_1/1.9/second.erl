-module(second).

-export([hypotenuse/2, perimeter/2, area/2]).

%% Returns the size of the hypotenuse of a right-angled triangle,
%% given the lengths of the two other sides.
-spec hypotenuse(X :: number(), Y :: number()) -> number().
hypotenuse(X, Y) ->
    math:sqrt(first:square(X) + first:square(Y)).

%% Returns the perimeter of a right-angled triangle,
%% given the lengths of the two short sides.
-spec perimeter(X :: number(), Y :: number()) -> number().
perimeter(X, Y) ->
    X + Y + hypotenuse(X, Y).

%% Returns the area of a right-angled triangle,
%% given the lengths of the two short sides.
-spec area(X :: number(), Y :: number()) -> number().
area(X, Y) ->
    first:area(X, Y, hypotenuse(X, Y)).
