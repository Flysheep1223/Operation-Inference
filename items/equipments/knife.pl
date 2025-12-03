:- dynamic equipment/4. % equipment(Type, Name, X, Y)

% Initialize knife
init_knife(X, Y) :-
    assertz(equipment(knife, 'Dagger', X, Y)).

% Effect when picked up
apply_knife_effect :-
    player_atk(Atk),
    NewAtk is Atk + 5,
    retract(player_atk(Atk)),
    assert(player_atk(NewAtk)),
    format('~n[ITEM] You equipped a Dagger! Attack +5 (Total: ~w)~n', [NewAtk]).
