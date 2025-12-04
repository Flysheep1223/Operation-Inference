% Spawn items randomly
:- multifile treasure_value/3.
:- dynamic treasure_value/3.

spawn_items :-
    spawn_random_item(sword),
    spawn_random_item(knife),
    spawn_treasures.

spawn_random_item(Type) :-
    map_size(MaxX, MaxY),
    random_between(1, MaxX, X),
    random_between(1, MaxY, Y),
    (   \+ wall(X, Y), 
        \+ location(player, X, Y), 
        \+ equipment(_, _, X, Y), 
        \+ treasure(_, X, Y),
        \+ exit_pos(X, Y),
        \+ portal_pos(X, Y, _)
    ->  (Type = sword -> init_sword(X, Y) ; init_knife(X, Y))
    ;   spawn_random_item(Type) % Retry if position invalid
    ).

spawn_treasures :-
    spawn_random_treasure(diamond),
    spawn_random_treasure(gold),
    spawn_random_treasure(silver).

spawn_random_treasure(Type) :-
    map_size(MaxX, MaxY),
    random_between(1, MaxX, X),
    random_between(1, MaxY, Y),
    (   \+ wall(X, Y),
        \+ location(player, X, Y),
        \+ equipment(_, _, X, Y),
        \+ treasure(_, X, Y),
        \+ exit_pos(X, Y),
        \+ portal_pos(X, Y, _)
    ->  assert(treasure(Type, X, Y))
    ;   spawn_random_treasure(Type)
    ).

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
