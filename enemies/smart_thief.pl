:- dynamic smart_thief/5. % smart_thief(Name, X, Y, Atk, Stun)
:- use_module(library(process)).

init_smart_thief :-
    retractall(smart_thief(_, _, _, _, _)),
    spawn_smart_thief.

spawn_smart_thief :-
    map_size(MaxX, MaxY),
    random_between(1, MaxX, X),
    random_between(1, MaxY, Y),
    (   \+ wall(X, Y),
        \+ location(player, X, Y),
        \+ equipment(_, _, X, Y),
        \+ treasure(_, X, Y),
        \+ healthy_package(X, Y),
        \+ exit_pos(X, Y)
    ->  assertz(smart_thief('Smart Thief', X, Y, 15, 0))
    ;   spawn_smart_thief
    ).

smart_thief_tick :-
    smart_thief(Name, X, Y, Atk, Stun),
    (   Stun > 0
    ->  NewStun is Stun - 1,
        retract(smart_thief(Name, X, Y, Atk, Stun)),
        assertz(smart_thief(Name, X, Y, Atk, NewStun)),
        format('~n[Thief] ~w is recovering... (~w turns left)~n', [Name, NewStun])
    ;   find_nearest_treasure(X, Y, TX, TY)
    ->  generate_pddl_problem(X, Y, TX, TY),
        run_external_planner(NextX, NextY),
        move_thief(Name, X, Y, NextX, NextY, Atk)
    ;   random_move_thief(Name, X, Y, Atk) % No treasure, wander randomly
    ).
smart_thief_tick.

% --- PDDL Integration ---

generate_pddl_problem(SX, SY, GX, GY) :-
    open('pddl/problem.pddl', write, Stream),
    write(Stream, '(define (problem steal-treasure)\n'),
    write(Stream, '  (:domain grid-world)\n'),
    write(Stream, '  (:objects\n'),
    % Only generate relevant objects if needed, or abstract
    % For simplicity, we rely on the implicit connectivity logic below
    write(Stream, '  )\n'),
    
    write(Stream, '  (:init\n'),
    format(Stream, '    (at c_~w_~w)\n', [SX, SY]),
    
    % Generate connectivity for the whole map (or a window)
    % Generating full map connectivity:
    forall(wall(WX, WY), assert(is_wall_temp(WX, WY))),
    map_size(MaxX, MaxY),
    forall(
        (between(0, MaxX, X), between(0, MaxY, Y), \+ is_wall_temp(X, Y)),
        (
            (X1 is X+1, \+ is_wall_temp(X1, Y) -> format(Stream, '    (connected c_~w_~w c_~w_~w)\n', [X, Y, X1, Y]) ; true),
            (X2 is X-1, \+ is_wall_temp(X2, Y) -> format(Stream, '    (connected c_~w_~w c_~w_~w)\n', [X, Y, X2, Y]) ; true),
            (Y1 is Y+1, \+ is_wall_temp(X, Y1) -> format(Stream, '    (connected c_~w_~w c_~w_~w)\n', [X, Y, X, Y1]) ; true),
            (Y2 is Y-1, \+ is_wall_temp(X, Y2) -> format(Stream, '    (connected c_~w_~w c_~w_~w)\n', [X, Y, X, Y2]) ; true)
        )
    ),
    retractall(is_wall_temp(_, _)),
    
    write(Stream, '  )\n'),
    
    write(Stream, '  (:goal\n'),
    format(Stream, '    (at c_~w_~w)\n', [GX, GY]),
    write(Stream, '  )\n'),
    write(Stream, ')\n'),
    close(Stream).

run_external_planner(NextX, NextY) :-
    catch(
        process_create(path(python), ['pddl/pddl_solver.py', 'pddl/domain.pddl', 'pddl/problem.pddl'], [stdout(pipe(Out))]),
        E,
        (format('~n[ERROR] PDDL Planner failed: ~w~n', [E]), fail)
    ),
    read_line_to_string(Out, Output),
    close(Out),
    (   Output == end_of_file
    ->  format('~n[DEBUG] Planner returned no output.~n', []),
        NextX = -1, NextY = -1
    ;   
        % format('~n[DEBUG] Planner Output: ~w~n', [Output]),
        (   sub_string(Output, _, _, _, '(move')
        ->  split_string(Output, " )", " (", RawTokens),
            exclude(==(""), RawTokens, Tokens), % Remove empty tokens caused by trailing separators
            % Expected Tokens: ["move", "c_SX_SY", "c_NX_NY"]
            (   length(Tokens, 3), nth1(3, Tokens, DestStr)
            ->  split_string(DestStr, "_", "", Parts),
                (   length(Parts, 3), nth1(2, Parts, NXStr), nth1(3, Parts, NYStr)
                ->  number_string(NextX, NXStr),
                    number_string(NextY, NYStr)
                ;   NextX = -1, NextY = -1
                )
            ;   % Fallback: maybe there are more tokens or different format
                (   length(Tokens, N), N >= 3, nth1(3, Tokens, DestStr)
                ->  split_string(DestStr, "_", "", Parts),
                    (   length(Parts, 3), nth1(2, Parts, NXStr), nth1(3, Parts, NYStr)
                    ->  number_string(NextX, NXStr),
                        number_string(NextY, NYStr)
                    ;   NextX = -1, NextY = -1
                    )
                ;   NextX = -1, NextY = -1
                )
            )
        ;   % format('~n[DEBUG] Planner did not return a move. Output: ~w~n', [Output]),
            NextX = -1, NextY = -1
        )
    ).

% --- Thief Logic ---

move_thief(Name, X, Y, NextX, NextY, Atk) :-
    NextX \= -1,
    \+ location(player, NextX, NextY), % Do not bump into player (combat handled in player turn or separate check)
    retract(smart_thief(Name, X, Y, Atk, 0)),
    assertz(smart_thief(Name, NextX, NextY, Atk, 0)),
    format('~n[Thief] ~w sneaks to (~w, ~w).~n', [Name, NextX, NextY]),
    check_thief_steal(NextX, NextY).
    
move_thief(Name, _, _, _, _, _) :-
    format('~n[Thief] ~w is planning his next move...~n', [Name]).

check_thief_steal(X, Y) :-
    treasure(Type, X, Y),
    retract(treasure(Type, X, Y)),
    treasure_value(Type, Value, ItemName),
    format('~n*** ALERT! A Thief stole ~w! (Value: ~w) ***~n', [ItemName, Value]),
    !.
check_thief_steal(_, _).

find_nearest_treasure(X, Y, TX, TY) :-
    findall([Dist, TrX, TrY], (treasure(_, TrX, TrY), distance(X, Y, TrX, TrY, Dist)), Treasures),
    sort(Treasures, [[_, TX, TY] | _]).

random_move_thief(Name, X, Y, Atk) :-
    random_permutation([up, down, left, right], [Dir|_]),
    (   Dir = up, NY is Y + 1, NX = X
    ;   Dir = down, NY is Y - 1, NX = X
    ;   Dir = left, NX is X - 1, NY = Y
    ;   Dir = right, NX is X + 1, NY = Y
    ),
    \+ wall(NX, NY),
    \+ location(player, NX, NY),
    retract(smart_thief(Name, X, Y, Atk, 0)),
    assertz(smart_thief(Name, NX, NY, Atk, 0)),
    format('~n[Thief] ~w wanders to (~w, ~w).~n', [Name, NX, NY]).
random_move_thief(_, _, _, _) :- !. % Stay still if blocked
