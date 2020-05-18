% A solution to the Package delivery problem
% By Viktor From

% State consists of 4 locations
% (robot location, steel key location, brass key location, package location)

% A three-place relation:
% move(state1, Move, state2)
% State1 -> Move -> State2
   

% Grab steel key, move to room 2.
move(state(r1, inventory, BrassKey, Package),
     walk(r1, r2),
     state(r2, inventory, BrassKey, Package)). 

% Grab brass key, move to room 1.
move(state(r2, inventory, inventory, Package),
     walk(r2, r1),
     state(r1, inventory, inventory, Package)). 

% Drop steel key, move to room 3.
move(state(r1, SteelKey, inventory, Package),
     walk(r1, r3),
     state(r3, SteelKey, inventory, Package)). 

% Grab package, move to room 1.
move(state(r3, SteelKey, inventory, inventory),
     walk(r3, r1),
     state(r1, SteelKey, inventory, inventory)). 

% Grab package, move to room 1.
move(state(r3, SteelKey, inventory, inventory),
     walk(r3, r1),
     state(r1, SteelKey, inventory, inventory)). 

% Drop brass key, grab steel key, move to room 2.
move(state(r1, inventory, BrassKey, inventory),
     walk(r1, r2),
     state(r2, inventory, BrassKey, inventory)). 

canget(state( _, _, _, has)).       

%canget(State1) :- move(State1, Move, State2), canget(State2). 

canget(state(r1, inventory, atwindow, hasnot)).

%solveR(State, N, Trace).
