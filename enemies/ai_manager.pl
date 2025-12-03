:- dynamic chaser/3, random_walker/5.

% Initialize enemies
init_enemies :-
    init_chaser(10, 2),
    init_random_walker(40, 10).

% Global tick for all enemies
enemies_tick :-
    chaser_tick,
    random_walker_tick.
