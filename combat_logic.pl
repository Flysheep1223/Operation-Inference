
% --- Combat Logic ---

check_combat :-
    location(player, PX, PY),
    check_all_chasers(PX, PY),
    check_all_walkers(PX, PY),
    check_hidden_bee(PX, PY).

check_all_chasers(PX, PY) :-
    chaser(Name, CX, CY, Atk, Stun),
    distance(PX, PY, CX, CY, Dist),
    Dist =< 1, % 8 surrounding cells or same cell
    resolve_combat(player, Name, Atk, Stun, chaser, CX, CY),
    fail.
check_all_chasers(_, _).

check_all_walkers(PX, PY) :-
    random_walker(Name, RX, RY, _, _, Atk, Stun),
    distance(PX, PY, RX, RY, Dist),
    Dist =< 1,
    resolve_combat(player, Name, Atk, Stun, random_walker, RX, RY),
    fail.
check_all_walkers(_, _).

check_hidden_bee(PX, PY) :-
    hidden_bee(Name, BX, BY, Atk, Stun, _),
    distance(PX, PY, BX, BY, Dist),
    Dist =< 1,
    resolve_combat(player, Name, Atk, Stun, hidden_bee, BX, BY).
check_hidden_bee(_, _).

resolve_combat(_, Name, EnemyAtk, Stun, Type, EX, EY) :-
    player_atk(PlayerAtk),
    format('~n[COMBAT] Enemy ~w (Atk: ~w) is nearby!~n', [Name, EnemyAtk]),
    (   PlayerAtk >= EnemyAtk
    ->  format('*** VICTORY! You defeated ~w! ***~n', [Name]),
        remove_enemy(Type, Name)
    ;   % Player weaker
        Damage is EnemyAtk - PlayerAtk,
        format('*** DEFEAT! You took ~w damage! ***~n', [Damage]),
        decrease_health(Damage),
        stun_enemy(Type, Name, EX, EY, EnemyAtk, Stun)
    ).

remove_enemy(chaser, Name) :-
    retract(chaser(Name, _, _, _, _)),
    format('~w has been removed.~n', [Name]).
remove_enemy(random_walker, Name) :-
    retract(random_walker(Name, _, _, _, _, _, _)),
    format('~w has been removed.~n', [Name]).
remove_enemy(hidden_bee, Name) :-
    retract(hidden_bee(Name, _, _, _, _, _)),
    retractall(bee_spike(_, _)), % Clear spikes when boss dies
    format('~w has been removed.~n', [Name]).

stun_enemy(chaser, Name, X, Y, Atk, _) :-
    retract(chaser(Name, X, Y, Atk, _)),
    assertz(chaser(Name, X, Y, Atk, 2)), % Stun for 2 turns
    format('~w is stunned for 2 turns.~n', [Name]).
stun_enemy(random_walker, Name, X, Y, Atk, _) :-
    retract(random_walker(Name, X, Y, Dx, Dy, Atk, _)),
    assertz(random_walker(Name, X, Y, Dx, Dy, Atk, 2)),
    format('~w is stunned for 2 turns.~n', [Name]).
stun_enemy(hidden_bee, Name, X, Y, Atk, _) :-
    retract(hidden_bee(Name, X, Y, Atk, _, Cooldown)),
    assertz(hidden_bee(Name, X, Y, Atk, 2, Cooldown)),
    format('~w is stunned for 2 turns.~n', [Name]).

distance(X1, Y1, X2, Y2, Dist) :-
    Dx is abs(X1 - X2),
    Dy is abs(Y1 - Y2),
    Dist is max(Dx, Dy). % Chebyshev distance

decrease_health(Amount) :-
    health(H),
    H > 0,
    H_New is H - Amount,
    retract(health(H)),
    (H_New =< 0 -> 
        assert(health(0)), 
        end_game_low_health 
    ;   assert(health(H_New))
        % Removed inline print: format('Health dropped to ~w.~n', [H_New])
    ).
