:- dynamic healthy_package/2.

% Spawn a healthy package at X, Y
spawn_healthy_package(X, Y) :-
    assertz(healthy_package(X, Y)).

% Effect when picked up
pickup_healthy_package(X, Y) :-
    healthy_package(X, Y),
    health(H),
    H < 100, % Only pick up if health is not full? Or always pick up? Requirement says "immediately restore", implying pick up.
    % Let us assume we pick it up even if full health (wasted), or maybe check condition.
    % Requirement: "Pick up and heal immediately" -> Implies action on pickup.
    
    retract(healthy_package(X, Y)),
    NewH is min(100, H + 40),
    retract(health(H)),
    assert(health(NewH)),
    format('~n*** You used a Healthy Package! (+40 HP, Current: ~w) ***~n', [NewH]).
