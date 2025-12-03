:- dynamic random_walker/5.

% Initialize random walker
init_random_walker(X, Y) :-
    retractall(random_walker(_,_,_,_,_)),
    % Start with a random initial direction
    random_member([Dx, Dy], [[0,1], [0,-1], [1,0], [-1,0]]),
    assertz(random_walker(walker1, X, Y, Dx, Dy)).

% Random Walker Logic
random_walker_tick :-
    current_predicate(game_over/0), game_over, !.
random_walker_tick :-
    \+ random_walker(_, _, _, _, _), !.

random_walker_tick :-
    random_walker(Name, X, Y, LastDx, LastDy),
    
    % 1. Try to maintain momentum (80% chance)
    MomX is X + LastDx, 
    MomY is Y + LastDy,
    (   (LastDx \= 0 ; LastDy \= 0),
        maybe(0.8),
        in_bounds(MomX, MomY),
        \+ wall_check([MomX, MomY])
    ->  NewX = MomX, NewY = MomY, NewDx = LastDx, NewDy = LastDy
    ;   % 2. If blocked or change direction: pick new valid move
        findall([NX, NY], (neighbor([X, Y], [NX, NY]), \+ wall_check([NX, NY])), AllNeighbors),
        
        % Filter out the tile we just came from to avoid jitter (Right-Left-Right),
        % unless it's a dead end.
        BackX is X - LastDx, BackY is Y - LastDy,
        exclude(is_pos(BackX, BackY), AllNeighbors, ForwardNeighbors),
        
        (   ForwardNeighbors \= [] -> Candidates = ForwardNeighbors
        ;   Candidates = AllNeighbors
        ),
        
        (   Candidates \= []
        ->  random_member([NewX, NewY], Candidates),
            NewDx is NewX - X,
            NewDy is NewY - Y
        ;   % Completely stuck
            NewX = X, NewY = Y, NewDx = LastDx, NewDy = LastDy
        )
    ),
    
    retract(random_walker(Name, _, _, _, _)),
    assertz(random_walker(Name, NewX, NewY, NewDx, NewDy)),
    
    % Check collision with player
    check_walker_capture(NewX, NewY).

check_walker_capture(X, Y) :-
    location(player, PX, PY),
    (   X =:= PX, Y =:= PY
    ->  format('~n*** You were bumped by the Random Walker! GAME OVER ***~n'),
        assert(game_over),
        end_game
    ;   true
    ).

is_pos(X, Y, [X, Y]).

in_bounds(X, Y) :-
    map_size(MaxX, MaxY),
    X >= 0, X =< MaxX, 
    Y >= 0, Y =< MaxY.

% Shared predicates used here (ensure they are available or redefined)
% neighbor/2 and wall_check/1 are also in bfs_chaser.pl. 
% Since Prolog modules share the global database for dynamic predicates like wall/2, 
% we can rely on the game engine or redefine helpers locally.
% For safety, we redefine local helpers here to keep the file self-contained or rely on game_engine loading.

neighbor([X, Y], [NX, NY]) :-
    ( NX is X + 1, NY = Y
    ; NX is X - 1, NY = Y
    ; NX = X, NY is Y + 1
    ; NX = X, NY is Y - 1
    ),
    map_size(MaxX, MaxY),
    NX >= 0, NX =< MaxX, NY >= 0, NY =< MaxY.

wall_check([X, Y]) :- wall(X, Y).
