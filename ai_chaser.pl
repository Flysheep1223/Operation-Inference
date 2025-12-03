:- dynamic chaser/3, random_walker/5.

% Initialize enemies
init_enemies :-
    init_chaser(10, 2),
    init_random_walker(40, 10).

init_chaser(X, Y) :-
    retractall(chaser(_,_,_)),
    assertz(chaser(chaser1, X, Y)).

init_random_walker(X, Y) :-
    retractall(random_walker(_,_,_,_,_)),
    % Start with a random initial direction
    random_member([Dx, Dy], [[0,1], [0,-1], [1,0], [-1,0]]),
    assertz(random_walker(walker1, X, Y, Dx, Dy)).

% Global tick for all enemies
enemies_tick :-
    chaser_tick,
    random_walker_tick.

% --- Random Walker Logic ---
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
        maybe(0.6),
        in_bounds(MomX, MomY),
        \+ wall_check([MomX, MomY])
    ->  NewX = MomX, NewY = MomY, NewDx = LastDx, NewDy = LastDy
    ;   % 2. If blocked or change direction: pick new valid move
        findall([NX, NY], (neighbor([X, Y], [NX, NY]), \+ wall_check([NX, NY])), AllNeighbors),
        
        % Filter out the tile we just came from to avoid jitter (Right-Left-Right),
        % unless it is a dead end.
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

% --- Chaser Logic ---
chaser_tick :-
    current_predicate(game_over/0), game_over, !.
chaser_tick :-
    \+ chaser(_, _, _), !.
chaser_tick :-
    chaser(Name, CX, CY),
    location(player, PX, PY),
    
    (   CX =:= PX, CY =:= PY
    ->  handle_capture
    ;   % Run BFS
        (   solve_bfs([CX, CY], [PX, PY], Path)
        ->  Path = [_, [NextX, NextY] | _], % First element is Start, second is Next
            move_chaser(Name, NextX, NextY),
            (   NextX =:= PX, NextY =:= PY
            ->  handle_capture
            ;   true
            )
        ;   format('~n[BFS] Chaser blocked or cannot find path.~n')
        )
    ).

move_chaser(Name, X, Y) :-
    retract(chaser(Name, _, _)),
    assertz(chaser(Name, X, Y)),
    format('~n[BFS] Chaser moved to (~w, ~w).~n', [X, Y]).

handle_capture :-
    format('~n*** Chaser caught you! GAME OVER ***~n'),
    assert(game_over),
    end_game.

% --- BFS Implementation ---

% solve_bfs(Start, Goal, Path)
solve_bfs(Start, Goal, Path) :-
    bfs([[Start]], Goal, [Start], Path).

% Base case: Goal reached
bfs([[Goal|RestPath]|_], Goal, _, Path) :-
    reverse([Goal|RestPath], Path), !.

% Recursive step
bfs([CurrentPath|Queue], Goal, Visited, FinalPath) :-
    CurrentPath = [CurrentNode|_],
    findall(
        [NextNode, CurrentNode|RestPath],
        (
            CurrentPath = [_|RestPath],
            neighbor(CurrentNode, NextNode),
            \+ member(NextNode, Visited),
            \+ wall_check(NextNode)
        ),
        NewPaths
    ),
    extract_heads(NewPaths, NewNodes),
    append(Visited, NewNodes, NewVisited),
    append(Queue, NewPaths, NewQueue),
    bfs(NewQueue, Goal, NewVisited, FinalPath).

extract_heads([], []).
extract_heads([[Head|_]|Tail], [Head|Heads]) :-
    extract_heads(Tail, Heads).

neighbor([X, Y], [NX, NY]) :-
    ( NX is X + 1, NY = Y
    ; NX is X - 1, NY = Y
    ; NX = X, NY is Y + 1
    ; NX = X, NY is Y - 1
    ),
    map_size(MaxX, MaxY),
    NX >= 0, NX =< MaxX, NY >= 0, NY =< MaxY.

wall_check([X, Y]) :- wall(X, Y).
