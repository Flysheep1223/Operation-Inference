:- dynamic chaser/6, random_walker/7, hidden_bee/6, timid_watched/5.

% Initialize enemies
init_enemies :-
    init_chaser,
    init_random_walker, % No parameters needed now
    init_hidden_bee,
    init_timid_watched,
    init_smart_thief.

% Global tick for all enemies
enemies_tick :-
    chaser_tick,
    random_walker_tick,
    hidden_bee_tick,
    timid_watched_tick,
    smart_thief_tick.
