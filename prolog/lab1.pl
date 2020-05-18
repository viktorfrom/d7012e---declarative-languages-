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



% Grab steel key
move(state(r1, inventory, BrassKey, Package),
     grab = "Picked up steel key.",
     state(r1, inventory, BrassKey, Package)).

% Grab brass key
move(state(r2, SteelKey, inventory, Package),
     grab = "Picked up brass key.",
     state(r2, SteelKey, inventory, Package)). 

% Grab package.
move(state(r3, SteelKey, BrassKey, inventory),
     grab = "Picked up package.",
     state(r3, SteelKey, BrassKey, inventory)). 





% Drop steel key.
move(state(_, inventory, BrassKey, Package),
     drop,
     state(_, inventory, BrassKey, Package)). 

% Drop brass key.
move(state(_, SteelKey, inventory, Package),
     drop,
     state(_, SteelKey, inventory, Package)). 

% Drop package.
move(state(_, SteelKey, BrassKey, inventory),
     drop,
     state(_, SteelKey, BrassKey, inventory)). 

canget(state( _, _, _, has)).       

%canget(State1) :- move(State1, Move, State2), canget(State2). 

canget(state(r1, inventory, atwindow, hasnot)).

%solveR(State, N, Trace).
