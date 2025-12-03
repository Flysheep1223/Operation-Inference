:- dynamic chaser/3.
:- use_module(ai_utils).

% Initialize chaser
init_chaser(X, Y) :-
    retractall(chaser(_,_,_)),
    assertz(chaser(chaser1, X, Y)).

% Chaser Logic
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
