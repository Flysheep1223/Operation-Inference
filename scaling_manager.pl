% --- Difficulty Scaling ---

% Increase turn count and check for scaling
check_scaling :-
    turn_count(N),
    NewN is N + 1,
    retract(turn_count(N)),
    assert(turn_count(NewN)),
    
    (   NewN mod 10 =:= 0
    ->  scale_enemies
    ;   true
    ).

scale_enemies :-
    turn_count(Turn),
    scaling_level(Lvl),
    NewLvl is Lvl + 1,
    retract(scaling_level(Lvl)),
    assert(scaling_level(NewLvl)),
    format('~n[WARNING] The enemies are growing stronger... (Turn ~w, Level ~w)~n', [Turn, NewLvl]),
    scale_chasers,
    scale_walkers,
    scale_timid_watched.

scale_chasers :-
    findall([Name, X, Y, Atk, Stun], chaser(Name, X, Y, Atk, Stun), Chasers),
    update_chasers(Chasers).

update_chasers([]).
update_chasers([[Name, X, Y, Atk, Stun] | Rest]) :-
    NewAtk is floor(Atk * 1.1), % Increase by 10%
    retract(chaser(Name, X, Y, Atk, Stun)),
    assertz(chaser(Name, X, Y, NewAtk, Stun)),
    update_chasers(Rest).

scale_walkers :-
    findall([Name, X, Y, Dx, Dy, Atk, Stun], random_walker(Name, X, Y, Dx, Dy, Atk, Stun), Walkers),
    update_walkers(Walkers).

update_walkers([]).
update_walkers([[Name, X, Y, Dx, Dy, Atk, Stun] | Rest]) :-
    NewAtk is floor(Atk * 1.1),
    retract(random_walker(Name, X, Y, Dx, Dy, Atk, Stun)),
    assertz(random_walker(Name, X, Y, Dx, Dy, NewAtk, Stun)),
    update_walkers(Rest).

scale_timid_watched :-
    findall([Name, X, Y, Atk, Stun], timid_watched(Name, X, Y, Atk, Stun), Bosses),
    update_timid_watched(Bosses).

update_timid_watched([]).
update_timid_watched([[Name, X, Y, Atk, Stun] | Rest]) :-
    NewAtk is floor(Atk * 1.1),
    retract(timid_watched(Name, X, Y, Atk, Stun)),
    assertz(timid_watched(Name, X, Y, NewAtk, Stun)),
    update_timid_watched(Rest).
