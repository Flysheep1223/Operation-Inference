% Spawn items randomly
spawn_items :-
    spawn_random_item(sword),
    spawn_random_item(knife).

spawn_random_item(Type) :-
    map_size(MaxX, MaxY),
    random_between(1, MaxX, X),
    random_between(1, MaxY, Y),
    (   \+ wall(X, Y), \+ location(player, X, Y), \+ equipment(_, _, X, Y)
    ->  (Type = sword -> init_sword(X, Y) ; init_knife(X, Y))
    ;   spawn_random_item(Type) % Retry if position invalid
    ).

% Check for item pickup
check_items(X, Y) :-
    equipment(Type, Name, X, Y),
    retract(equipment(Type, Name, X, Y)),
    format('~n*** You found a ~w! ***~n', [Name]),
    (Type = sword -> apply_sword_effect ; apply_knife_effect),
    !.
check_items(_, _).
