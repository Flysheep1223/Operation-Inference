:- module(ai_utils, [neighbor/2, wall_check/1, in_bounds/2]).

neighbor([X, Y], [NX, NY]) :-
    ( NX is X + 1, NY = Y
    ; NX is X - 1, NY = Y
    ; NX = X, NY is Y + 1
    ; NX = X, NY is Y - 1
    ),
    in_bounds(NX, NY).

in_bounds(X, Y) :-
    map_size(MaxX, MaxY),
    X >= 0, X =< MaxX, 
    Y >= 0, Y =< MaxY.

wall_check([X, Y]) :- wall(X, Y).
