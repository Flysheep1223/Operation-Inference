:- dynamic random_walker/7.
:- use_module(ai_utils).

% Initialize random walker
init_random_walker(X, Y) :-
    retractall(random_walker(_,_,_,_,_,_,_)),
    % Start with a random initial direction
    random_member([Dx, Dy], [[0,1], [0,-1], [1,0], [-1,0]]),
    assertz(random_walker(walker1, X, Y, Dx, Dy, 20, 0)).

% Random Walker Logic
random_walker_tick :-
    current_predicate(game_over/0), game_over, !.
random_walker_tick :-
    \+ random_walker(_, _, _, _, _, _, _), !.

random_walker_tick :-
    random_walker(Name, X, Y, LastDx, LastDy, Atk, Stun),
    
    (   Stun > 0
    ->  NewStun is Stun - 1,
        retract(random_walker(Name, X, Y, LastDx, LastDy, Atk, Stun)),
        assertz(random_walker(Name, X, Y, LastDx, LastDy, Atk, NewStun)),
        format('~n[Enemy] ~w is stunned for ~w more turns.~n', [Name, NewStun])
    ;   % 1. Try to maintain momentum (80% chance)
        MomX is X + LastDx, 
        MomY is Y + LastDy,
        (   (LastDx \= 0 ; LastDy \= 0),
            maybe(0.5),
            in_bounds(MomX, MomY),
            \+ wall_check([MomX, MomY])
        ->  NewX = MomX, NewY = MomY, NewDx = LastDx, NewDy = LastDy
        ;   % 2. If blocked or change direction: pick new valid move
            findall([NX, NY], (neighbor([X, Y], [NX, NY]), \+ wall_check([NX, NY])), AllNeighbors),
            
            % Filter out the tile we just came from to avoid jitter (Right-Left-Right),
            % unless it's a dead end.
            BackX is X - LastDx, BackY is Y - LastDy,
            exclude(is_pos(BackX, BackY), AllNeighbors, ForwardNeighbors),
            
            (   ForwardNeighbors \= [] -> Candidates = ForwardNeighbors
            ;   Candidates = AllNeighbors
            ),
            
            (   Candidates \= []
            ->  random_member([NewX, NewY], Candidates),
                NewDx is NewX - X,
                NewDy is NewY - Y
            ;   % Completely stuck
                NewX = X, NewY = Y, NewDx = LastDx, NewDy = LastDy
            )
        ),
        
        retract(random_walker(Name, _, _, _, _, _, _)),
        assertz(random_walker(Name, NewX, NewY, NewDx, NewDy, Atk, Stun))
    ).

is_pos(X, Y, [X, Y]).
