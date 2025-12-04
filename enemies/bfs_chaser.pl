:- dynamic chaser/6. % chaser(Name, X, Y, Atk, Stun, Active)
:- use_module(ai_utils).

% Initialize chaser at [10, 8] and inactive (0)
init_chaser :-
    retractall(chaser(_,_,_,_,_,_)),
    assertz(chaser(chaser1, 10, 8, 10, 0, 0)).

% Chaser Logic
chaser_tick :-
    current_predicate(game_over/0), game_over, !.
chaser_tick :-
    \+ chaser(_, _, _, _, _, _), !.
chaser_tick :-
    chaser(Name, CX, CY, Atk, Stun, Active),
    
    % Check activation condition if not active yet
    (   Active =:= 0
    ->  location(player, PX, PY),
        (   (PX =:= 16, (PY =:= 7 ; PY =:= 8 ; PY =:= 9))
        ->  NewActive = 1,
            format('~n[BFS] Chaser AWAKENED! Hunting player...~n')
        ;   NewActive = 0
        ),
        retract(chaser(Name, CX, CY, Atk, Stun, Active)),
        assertz(chaser(Name, CX, CY, Atk, Stun, NewActive))
    ;   NewActive = 1
    ),
    
    (   NewActive =:= 1
    ->  (   Stun > 0
        ->  NewStun is Stun - 1,
            retract(chaser(Name, CX, CY, Atk, Stun, NewActive)),
            assertz(chaser(Name, CX, CY, Atk, NewStun, NewActive)),
            format('~n[Enemy] ~w is stunned for ~w more turns.~n', [Name, NewStun])
        ;   location(player, PX, PY),
            (   CX =:= PX, CY =:= PY
            ->  true % Already on top of player
            ;   % Run BFS
                (   solve_bfs([CX, CY], [PX, PY], Path)
                ->  Path = [_, [NextX, NextY] | _],
                    move_chaser(Name, NextX, NextY, Atk, Stun, NewActive)
                ;   format('~n[BFS] Chaser blocked or cannot find path.~n')
                )
            )
        )
    ;   true % Inactive: Do nothing
    ).

move_chaser(Name, X, Y, Atk, Stun, Active) :-
    retract(chaser(Name, _, _, _, _, _)),
    assertz(chaser(Name, X, Y, Atk, Stun, Active)),
    format('~n[BFS] Chaser moved to (~w, ~w).~n', [X, Y]).

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
