% Spawn items randomly
:- multifile treasure_value/3.
:- dynamic treasure_value/3.

spawn_items :-
    spawn_random_equipments_from_areas,
    spawn_treasures.

spawn_random_equipments_from_areas :-
    equipment_spawn_area(Type, XMin, XMax, YMin, YMax, Count),
    spawn_n_equipments(Type, Count, XMin, XMax, YMin, YMax),
    fail.
spawn_random_equipments_from_areas.

spawn_n_equipments(_, 0, _, _, _, _) :- !.
spawn_n_equipments(Type, N, XMin, XMax, YMin, YMax) :-
    random_between(XMin, XMax, X),
    random_between(YMin, YMax, Y),
    (   \+ wall(X, Y), 
        \+ location(player, X, Y), 
        \+ equipment(_, _, X, Y), 
        \+ treasure(_, X, Y),
        \+ healthy_package(X, Y),
        \+ random_walker(_, X, Y, _, _, _, _),
        \+ exit_pos(X, Y),
        \+ portal_pos(X, Y, _)
    ->  (Type = sword -> init_sword(X, Y) ; init_knife(X, Y)),
        N1 is N - 1,
        spawn_n_equipments(Type, N1, XMin, XMax, YMin, YMax)
    ;   spawn_n_equipments(Type, N, XMin, XMax, YMin, YMax) % Retry
    ).

spawn_treasures :-
    spawn_n_treasures(diamond, 1),
    spawn_n_treasures(gold, 3),
    spawn_n_treasures(silver, 6).

spawn_n_treasures(_, 0) :- !.
spawn_n_treasures(Type, N) :-
    spawn_random_treasure(Type),
    N1 is N - 1,
    spawn_n_treasures(Type, N1).

spawn_random_treasure(Type) :-
    map_size(MaxX, MaxY),
    random_between(1, MaxX, X),
    random_between(1, MaxY, Y),
    (   \+ wall(X, Y),
        \+ location(player, X, Y),
        \+ equipment(_, _, X, Y),
        \+ treasure(_, X, Y),
        \+ healthy_package(X, Y),
        \+ random_walker(_, X, Y, _, _, _, _),
        \+ exit_pos(X, Y),
        \+ portal_pos(X, Y, _),
        \+ in_excluded_zone(X, Y)
    ->  assert(treasure(Type, X, Y))
    ;   spawn_random_treasure(Type)
    ).

in_excluded_zone(X, Y) :-
    X >= 24, X =< 36,
    Y >= 10, Y =< 22.

% Check for item pickup
check_items(X, Y) :-
    equipment(Type, Name, X, Y),
    retract(equipment(Type, Name, X, Y)),
    format('~n*** You found a ~w! ***~n', [Name]),
    (Type = sword -> apply_sword_effect ; apply_knife_effect),
    !.

check_items(X, Y) :-
    treasure(Type, X, Y),
    retract(treasure(Type, X, Y)),
    treasure_value(Type, Value, Name),
    score(CurrentScore),
    NewScore is CurrentScore + Value,
    retract(score(CurrentScore)),
    assert(score(NewScore)),
    format('~n*** You found ~w! (+~w points) ***~n', [Name, Value]),
    !.

check_items(_, _).
