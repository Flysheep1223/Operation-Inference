:- dynamic wall/2, location/3, game_over/0, health/1, player_atk/1, turn_count/1, scaling_level/1, score/1.
:- dynamic map_size/2, exit_pos/2, health_zone/2, map_segment/2, spawn_pos/2, portal_pos/3, health_spawn_area/5, equipment_spawn_area/6.
:- dynamic equipment/4, treasure/3.
:- use_module(library(readutil)).
:- [enemies/ai_manager].
:- [enemies/bfs_chaser].
:- [enemies/random_walker].
:- [enemies/hidden_bee].
:- [enemies/timid_watched].
:- [enemies/smart_thief].
:- [combat_logic].
:- [items/equipments/sword].
:- [items/equipments/knife].
:- [items/tools/heathy_package].
:- [items/treasures/diamond].
:- [items/treasures/gold].
:- [items/treasures/silver].
:- [items/item_manager].
:- [scaling_manager].

% --- Map Loading Logic ---
load_level(LevelFile) :-
    retractall(wall(_, _)),
    retractall(map_size(_, _)),
    retractall(exit_pos(_, _)),
    retractall(health_zone(_, _)),
    retractall(health_spawn_area(_, _, _, _, _)),
    retractall(equipment_spawn_area(_, _, _, _, _, _)),
    retractall(map_segment(_, _)),
    retractall(spawn_pos(_, _)),
    retractall(portal_pos(_, _, _)),
    retractall(healthy_package(_, _)), % Clear old packages
    atom_concat('maps/', LevelFile, Path),
    consult(Path),
    generate_walls,
    init_healthy_packages. % Spawn new packages from map data

init_healthy_packages :-
    findall([X, Y], health_zone(X, Y), Zones),
    spawn_packages_from_zones(Zones),
    spawn_random_packages_from_areas.

spawn_random_packages_from_areas :-
    health_spawn_area(XMin, XMax, YMin, YMax, Count),
    spawn_n_packages(Count, XMin, XMax, YMin, YMax),
    fail.
spawn_random_packages_from_areas.

spawn_n_packages(0, _, _, _, _) :- !.
spawn_n_packages(N, XMin, XMax, YMin, YMax) :-
    random_between(XMin, XMax, X),
    random_between(YMin, YMax, Y),
    (   \+ wall(X, Y), \+ healthy_package(X, Y), \+ location(player, X, Y)
    ->  spawn_healthy_package(X, Y),
        N1 is N - 1,
        spawn_n_packages(N1, XMin, XMax, YMin, YMax)
    ;   spawn_n_packages(N, XMin, XMax, YMin, YMax) % Retry
    ).

spawn_packages_from_zones([]).
spawn_packages_from_zones([[X, Y] | Rest]) :-
    spawn_healthy_package(X, Y),
    spawn_packages_from_zones(Rest).

generate_walls :-
    retractall(wall(_, _)),
    map_segment([X1, Y1], [X2, Y2]),
    (   Y1 = Y2 -> fill_horizontal_line(X1, Y1, X2)
    ;   X1 = X2 -> fill_vertical_line(X1, Y1, Y2)
    ;   true
    ),
    fail.
generate_walls :-
    format('~nMap loaded successfully.~n').

fill_horizontal_line(X1, Y, X2) :-
    min_max(X1, X2, X_Min, X_Max),
    between(X_Min, X_Max, X),
    assertz(wall(X, Y)),
    fail.
fill_horizontal_line(_, _, _).

fill_vertical_line(X, Y1, Y2) :-
    min_max(Y1, Y2, Y_Min, Y_Max),
    between(Y_Min, Y_Max, Y),
    assertz(wall(X, Y)),
    fail.
fill_vertical_line(_, _, _).

min_max(A, B, Min, Max) :-
    (A =< B -> Min = A, Max = B ; Min = B, Max = A).

:- discontiguous update_location/2.

:- discontiguous move/1.
:- discontiguous check_spike_collision/0.
:- discontiguous check_spike_collision/2.

% --- Game Logic ---
update_location(NewX, NewY) :-
    \+ wall(NewX, NewY),
    % Move logic first to allow stepping ONTO the portal
    retract(location(player, _, _)),
    assert(location(player, NewX, NewY)),
    % restore_health, % Removed old continuous health zone logic
    check_healthy_package(NewX, NewY), % Check for one-time package
    check_events(NewX, NewY),
    check_items(NewX, NewY),
    check_combat,
    check_scaling,
    !.

check_healthy_package(X, Y) :-
    healthy_package(X, Y),
    !,
    pickup_healthy_package(X, Y).
check_healthy_package(_, _).
update_location(_, _) :-
    fail.

check_events(X, Y) :-
    check_portal(X, Y),
    check_exit(X, Y).

check_portal(X, Y) :-
    portal_pos(X, Y, TargetMap),
    !,
    format('~n*** PORTAL ACTIVATED! Traveling to ~w... ***~n', [TargetMap]),
    change_map(TargetMap).
check_portal(_, _).

change_map(TargetMap) :-
    load_level(TargetMap),
    spawn_pos(SX, SY),
    retractall(location(player, _, _)),
    assert(location(player, SX, SY)),
    format('~nYou have arrived at the new area.~n'),
    show_map.

check_exit(X, Y) :-
    exit_pos(X, Y),
    \+ game_over,
    score(S),
    format('~n**************************************************~n'),
    format('*** CONGRATULATIONS! You reached the destination! ***~n'),
    format('*** FINAL SCORE: ~w ***~n', [S]),
    format('**************************************************~n'),
    assert(game_over),
    end_game.
check_exit(_, _).

restore_health :-
    location(player, X, Y),
    health_zone(X, Y, Amount, Message),
    health(H),
    H < 100,
    NewH is min(100, H + Amount),
    retract(health(H)),
    assert(health(NewH)),
    format('~n~w (Health +~w, Current: ~w)~n', [Message, Amount, NewH]),
    !.
restore_health.

end_game_low_health :-
    \+ game_over,
    format('~n*** You have died from your injuries... GAME OVER ***~n'),
    assert(game_over),
    end_game.

end_game :-
    format('~nType "start." to play again.~n').

% --- Input Loop ---
move(Direction) :-
    location(player, X, Y),
    map_size(MaxX, MaxY),
    (   Direction = up,     Y1 is Y + 1, Y1 =< MaxY, update_location(X, Y1)
    ;   Direction = down,   Y1 is Y - 1, Y1 >= 0, update_location(X, Y1)
    ;   Direction = left,   X1 is X - 1, X1 >= 0, update_location(X1, Y)
    ;   Direction = right,  X1 is X + 1, X1 =< MaxX, update_location(X1, Y)
    ),
    location(player, NewX, NewY),
    format('~nYou moved to (~w, ~w).~n', [NewX, NewY]),
    show_map,
    enemies_tick,
    % Re-check spike collision AFTER enemies move (for "walking into" or "spawned on top" cases)
    location(player, _FinalX, _FinalY),
    check_combat,
    check_spike_collision,
    !.

check_spike_collision :-
    location(player, X, Y),
    bee_spike(SX, SY),
    SX =:= X, SY =:= Y,
    format('~n*** OUCH! You stepped on a spike! (-5 HP) ***~n'),
    decrease_health(5),
    !.
move(_) :-
    format('~nCannot move in that direction (Invalid direction, blocked, or out of bounds)!~n'),
    show_map,
    enemies_tick,
    check_combat,
    check_spike_collision.

% --- Teleport (Debug/Cheat) ---
tp(NewX, NewY) :-
    map_size(MaxX, MaxY),
    NewX >= 0, NewX =< MaxX,
    NewY >= 0, NewY =< MaxY,
    update_location(NewX, NewY),
    format('~n--- TELEPORT SUCCESSFUL ---~n'),
    location(player, NewX_Actual, NewY_Actual),
    format('You are now at (~w, ~w).~n', [NewX_Actual, NewY_Actual]),
    show_map,
    enemies_tick,
    check_combat,
    !.

% --- Debug Commands ---
dehealthy(Amount) :-
    decrease_health(Amount),
    format('~n--- DEBUG: Reduced Health by ~w ---~n', [Amount]),
    show_map.

% --- Display ---
show_map :-
    location(player, X, Y),
    draw_map(X, Y),
    health(H),
    player_atk(Atk),
    score(S),
    format('Health: ~w | Attack: ~w | Score: ~w~n', [H, Atk, S]),
    (   bee_spike(X, Y)
    ->  format('~n*** OUCH! You stepped on a spike! (-5 HP) ***~n')
    ;   true
    ).

draw_map(PlayerX, PlayerY) :-
    map_size(MaxX, MaxY),
    turn_count(Turn),
    scaling_level(Lvl),
    format('~n+--- Map (Current Position: @) [Turn: ~w] [Enemy Lv: ~w] ---+~n', [Turn, Lvl]),
    between(0, MaxY, Y_index),
    Y is MaxY - Y_index,
    draw_row(0, MaxX, Y, PlayerX, PlayerY),
    fail.
draw_map(_, _) :-
    format('+----------------------------------------+~n').

draw_row(X, MaxX, _, _, _) :- X > MaxX, format('~n'), !.
draw_row(X, MaxX, Y, PlayerX, PlayerY) :-
    print_map_char(X, Y, PlayerX, PlayerY),
    NextX is X + 1,
    draw_row(NextX, MaxX, Y, PlayerX, PlayerY).

print_map_char(X, Y, PlayerX, PlayerY) :-
    X =:= PlayerX, Y =:= PlayerY,
    % Check if spike is UNDER player to render it?
    % But @ usually covers everything.
    % If we want to see if they overlap visually:
    (   bee_spike(X, Y)
    ->  format(' X') % Show 'X' if player is hit/overlapping spike
    ;   format(' @')
    ), !.

print_map_char(X, Y, _, _) :-
    chaser(_, CX, CY, _, _, _),
    number(CX), number(CY),
    CX =:= X, CY =:= Y,
    format(' C'), !.

print_map_char(X, Y, _, _) :-
    random_walker(_, RX, RY, _, _, _, _),
    number(RX), number(RY),
    RX =:= X, RY =:= Y,
    format(' R'), !.

print_map_char(X, Y, _, _) :-
    timid_watched(_, TX, TY, _, _),
    number(TX), number(TY),
    TX =:= X, TY =:= Y,
    format(' T'), !.

print_map_char(X, Y, _, _) :-
    smart_thief(_, SX, SY, _, _),
    number(SX), number(SY),
    SX =:= X, SY =:= Y,
    format(' I'), !.

print_map_char(X, Y, _, _) :-
    hidden_bee(_, BX, BY, _, _, _),
    number(BX), number(BY),
    BX =:= X, BY =:= Y,
    format(' B'), !.

print_map_char(X, Y, _, _) :-
    bee_spike(SX, SY),
    SX =:= X, SY =:= Y,
    format(' ^'), !.

print_map_char(X, Y, _, _) :-
    equipment(Type, _, EX, EY),
    EX =:= X, EY =:= Y,
    (Type = sword -> format(' S') ; format(' K')), !.

print_map_char(X, Y, _, _) :-
    healthy_package(HX, HY),
    HX =:= X, HY =:= Y,
    format(' H'), !.

print_map_char(X, Y, _, _) :-
    treasure(Type, TX, TY),
    TX =:= X, TY =:= Y,
    (Type = diamond -> format(' O')
    ; Type = gold -> format(' G')
    ; format(' V')
    ), !.

print_map_char(X, Y, _, _) :-
    exit_pos(X, Y),
    format(' $'), !.

print_map_char(X, Y, _, _) :-
    wall(X, Y),
    (   (is_wall(X, Y+1); is_wall(X, Y-1)),
        \+ (is_wall(X+1, Y); is_wall(X-1, Y)) -> format('|')
    ;   (is_wall(X+1, Y); is_wall(X-1, Y)),
        \+ (is_wall(X, Y+1); is_wall(X, Y-1)) -> format('-')
    ;   format(' +')
    ), !.

print_map_char(_, _, _, _) :-
    format(' .'), !.

is_wall(X, Y) :- wall(X, Y).

get_health_zone_char(_, _, _) :- fail.

% --- Main Loop ---
start_game :-
    load_level('level1.pl'),
    retractall(game_over),
    retractall(location(player, _, _)),
    retractall(health(_)),
    retractall(player_atk(_)),
    retractall(equipment(_,_,_,_)),
    retractall(treasure(_,_,_)),
    retractall(turn_count(_)),
    retractall(scaling_level(_)),
    retractall(score(_)),
    assert(health(100)),
    assert(player_atk(100)),
    assert(turn_count(0)),
    assert(score(0)),
    assert(scaling_level(0)),
    (   spawn_pos(SX, SY) -> assert(location(player, SX, SY))
    ;   assert(location(player, 30, 2))
    ),
    format('~nGame started!~n'),
    init_enemies,
    spawn_items,
    format('Controls: WASD to move, Q to quit.~n'),
    show_map,
    game_loop.

game_loop :-
    game_over, !.
game_loop :-
    get_action(Input),
    handle_input(Input),
    game_loop.

get_action(Input) :-
    % format('~nAction: '),
    get_single_char(Code),
    char_code(Input, Code),
    format('~w~n', [Input]).

handle_input('w') :- move(up).
handle_input('W') :- move(up).
handle_input('s') :- move(down).
handle_input('S') :- move(down).
handle_input('a') :- move(left).
handle_input('A') :- move(left).
handle_input('d') :- move(right).
handle_input('D') :- move(right).
handle_input('q') :- format('~nQuitting...~n'), assert(game_over).
handle_input('Q') :- format('~nQuitting...~n'), assert(game_over).
handle_input(_) :- format('~nUnknown command.~n').

% --- New Spike Check ---
% Mode: old (check against existing spikes before they move)
% Mode: new (check against moved spikes)

check_spike_collision(_, _) :- !.
check_spike_collision :-
    location(player, X, Y),
    bee_spike(SX, SY),
    SX =:= X, SY =:= Y,
    format('~n*** OUCH! You stepped on a spike! (-5 HP) ***~n'),
    decrease_health(5),
    !.
check_spike_collision.
check_spike_collision(_, _).
