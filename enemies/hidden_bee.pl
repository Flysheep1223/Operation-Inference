:- dynamic hidden_bee/6. % hidden_bee(Name, X, Y, Atk, Stun, AttackCooldown)
:- dynamic bee_spike/2. % bee_spike(X, Y)
:- use_module(ai_utils).

% Initialize Hidden Bee
init_hidden_bee :-
    retractall(hidden_bee(_,_,_,_,_,_)),
    retractall(bee_spike(_, _)),
    assertz(hidden_bee('Hidden Bee', 25, 30, 30, 0, 0)).

% Hidden Bee Logic - Does not move
hidden_bee_tick :-
    current_predicate(game_over/0), game_over, !.
hidden_bee_tick :-
    \+ hidden_bee(_, _, _, _, _, _), !.

hidden_bee_tick :-
    hidden_bee(Name, X, Y, Atk, Stun, Cooldown),
    
    % 1. Handle Stun
    (   Stun > 0
    ->  NewStun is Stun - 1,
        ResetCooldown = Cooldown, % Fix singleton variable
        format('~n[Boss] ~w is stunned for ~w more turns.~n', [Name, NewStun])
    ;   % 2. Handle Attack Logic (Every 4 turns)
        NewStun = 0,
        NewCooldown is Cooldown + 1,
        (   NewCooldown >= 4
        ->  spawn_spikes,
            ResetCooldown = 0,
            format('~n[Boss] ~w spits out a wave of spikes!~n', [Name])
        ;   ResetCooldown = NewCooldown
        )
    ),
    
    retract(hidden_bee(Name, X, Y, Atk, Stun, Cooldown)),
    assertz(hidden_bee(Name, X, Y, Atk, NewStun, ResetCooldown)),
    
    % 3. Move existing spikes
    move_spikes, !.

% --- Spike Mechanics ---

:- use_module(library(random)).

spawn_spikes :-
    % Generate random Y positions in range [27, 33] (Room height)
    findall(Y, between(27, 33, Y), AllY),
    random_permutation(AllY, RandomY),
    length(SpikeYs, 5),
    append(SpikeYs, _, RandomY),
    
    % Create spikes at X=26
    create_spikes(26, SpikeYs).

create_spikes(_, []).
create_spikes(X, [Y|Rest]) :-
    assertz(bee_spike(X, Y)),
    create_spikes(X, Rest).

move_spikes :-
    findall([X, Y], bee_spike(X, Y), Spikes),
    update_spikes(Spikes),
    !. % Ensure deterministic

update_spikes([]).
update_spikes([[X, Y] | Rest]) :-
    (   bee_spike(X, Y) % Verify spike still exists
    ->  retract(bee_spike(X, Y)),
        NewX is X + 1,
        (   NewX >= 35 % Wall/Door position
        ->  true % Spike disappears
        ;   assertz(bee_spike(NewX, Y))
        )
    ;   true % Already removed (rare race condition safety)
    ),
    update_spikes(Rest).

% Helper removed, collision handled in game_engine.pl
