-module(first).

-export([area/3]).

%% Calculates the area of a triangle.
area(A,B,C) ->
    S = (A+B+C)/2,
    math:sqrt(S*(S-A)*(S-B)*(S-C)).
