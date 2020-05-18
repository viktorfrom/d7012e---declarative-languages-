% A solution to the Package delivery problem
% By Viktor From

% State consists of 4 locations
% (robot location, steel key location, brass key location, package location)

% A three-place relation:
% move(state1, Move, state2)
% State1 -> Move -> State2


% Move from room 1 to room 2.
move(state(r1, inventory, BrassKey, Package),
     walk(r1, r2),
     state(r2, inventory, BrassKey, Package)). 

% Move from room 2 to room 1.
move(state(r2, inventory, BrassKey, Package),
     walk(r2, r1),
     state(r1, inventory, BrassKey, Package)). 

% Move from room 1 to room 3.
move(state(r1, SteelKey, inventory, Package),
     walk(r1, r3),
     state(r3, SteelKey, inventory, Package)). 

% Move from room 3 to room 1.
move(state(r3, SteelKey, inventory, Package),
     walk(r3, r1),
     state(r1, SteelKey, inventory, Package)). 



% Grab steel key.
move(state(Room, Room, BrassKey, Package),
     grab(steelKey),
     state = state(Room, inventory, BrassKey, Package)) :- 
          state \= state(_, inventory, inventory, inventory). 

% Grab brass key.
move(state(Room, SteelKey, Room, Package),
     grab(brassKey),
     state = state(Room, SteelKey, inventory, Package)) :-
          state \= state(_, inventory, inventory, inventory). 

% Grab package.
move(state(Room, SteelKey, BrassKey, Room),
     grab(package),
     state(Room, SteelKey, BrassKey, inventory)) :- 
          state \= state(_, inventory, inventory, inventory). 



% Drop steel key.
move(state(Room, inventory, BrassKey, Package),
     drop(steelKey),
     state(Room, Room, BrassKey, Package)). 

% Drop brass key.
move(state(Room, SteelKey, inventory, Package),
     drop(brassKey),
     state(Room, SteelKey, Room, Package)). 

% Drop package.
move(state(Room, SteelKey, BrassKey, inventory),
     drop(package),
     state(Room, SteelKey, BrassKey, Room)). 

canget(state( _, _, _, has)).       


% tests for grab.
% move(state(r1, r1, r2, r3), grab(steelKey), X).
% move(state(r2, r1, r2, r3), grab(brassKey), X).
% move(state(r3, r1, r2, r3), grab(package), X).

% tests for drop.
% move(state(r3, inventory, r2, r3), drop(steelKey), X).
% move(state(r1, r1, inventory, r3), drop(brassKey), X).
% move(state(r2, r1, r2, inventory), drop(package), X).

%canget(State1) :- move(State1, Move, State2), canget(State2). 

canget(state(r1, inventory, atwindow, hasnot)).

%solveR(State, N, Trace).
