% A solution to the Package delivery problem
% By Viktor From

% State consists of 4 locations
% (robot location, steel key location, brass key location, package location)

% A three-place relation:
% move(state1, Move, state2)
% State1 -> Move -> State2

% Move from room 1 to room 2.
move(State, Move, NextState) :- 
     State = state(r1, inventory, BrassKey, Package),
     Move = walk(r1, r2),
     NextState = state(r2, inventory, BrassKey, Package).

% Move from room 2 to room 1.
move(State, Move, NextState) :-
     State = state(r2, inventory, BrassKey, Package),
     Move = walk(r2, r1),
     NextState = state(r1, inventory, BrassKey, Package).

% Move from room 1 to room 3.
move(State, Move, NextState) :- 
     State = state(r1, SteelKey, inventory, Package),
     Move = walk(r1, r3),
     NextState = state(r3, SteelKey, inventory, Package).

% Move from room 3 to room 1.
move(State, Move, NewState) :-
     State = state(r3, SteelKey, inventory, Package),
     Move = walk(r3, r1),
     NewState = state(r1, SteelKey, inventory, Package).

% Grab steel key.
move(State, Move, NextState) :-
    State = state(Room, Room, BrassKey, Package),
    Move = grab(steelKey),
    NextState = state(Room, inventory, BrassKey, Package),
    NextState \= state(_, inventory, inventory, inventory).
    
    %\+(state(_, inventory, inventory, inventory) = NextState).

% Grab brass key.
move(State, Move, NextState) :-
    State = state(Room, SteelKey, Room, Package),
    Move = grab(brassKey),
    NextState = state(Room, SteelKey, inventory, Package),
    NextState \= state(_, inventory, inventory, inventory).
    %\+(state(_, inventory, inventory, inventory) = NextState).

% Grab package.
move(State, Move, NextState) :-
    State = state(Room, SteelKey, BrassKey, Room),
    Move = grab(package),
    NextState = state(Room, SteelKey, BrassKey, inventory),
    NextState \= state(_, inventory, inventory, inventory).
    %\+(state(_, inventory, inventory, inventory) = NextState).

% Drop steel key.
move(State, Move, NextState) :-
    State = state(Room, inventory, BrassKey, Package),
    Move = drop(steelKey),
    NextState = state(Room, Room, BrassKey, Package).

% Drop brass key.
move(State, Move, NextState) :- 
     State = state(Room, SteelKey, inventory, Package),
     Move = drop(brassKey),
     NextState = state(Room, SteelKey, Room, Package). 

% Drop package.
move(State, Move, NextState) :- 
     State = state(Room, SteelKey, BrassKey, inventory),
     Move = drop(package),
     NextState = state(Room, SteelKey, BrassKey, Room).

% test move.
% move(state(r1, inventory, r2, r3), walk(r1, r2), X).
% move(state(r2, inventory, r2, r3), walk(r2, r1), X).

% move(state(r1, r1, inventory, r3), walk(r1, r3), X).
% move(state(r3, r1, inventory, r3), walk(r3, r1), X).

% test grab.
% move(state(r1, r1, r2, r3), grab(steelKey), X).
% move(state(r2, r1, r2, r3), grab(brassKey), X).
% move(state(r3, r1, r2, r3), grab(package), X).

% test drop.
% move(state(r3, inventory, r2, r3), drop(steelKey), X).
% move(state(r1, r1, inventory, r3), drop(brassKey), X).
% move(state(r2, r1, r2, inventory), drop(package), X).

% test solveR.
% solveR(state(r1, r1, r2, r3), 12, X).

solveR(state(_, _, _, r2), _, ["done"]). 
solveR(State, N, [Move|Trace]) :-
    N > 0,
    move(State, Move, NextState),
    solveR(NextState, N - 1, Trace).