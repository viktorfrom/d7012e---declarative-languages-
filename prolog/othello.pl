/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: <TO BE FILLED IN BEFORE THE GRADING> 
%    Student user id  : <TO BE FILLED IN BEFORE THE GRADING> 
%
/* ------------------------------------------------------- */



%do not change the following line!
:- ensure_loaded('play.pl').


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 





% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    [.,.,1,2,.,.], 
	    [.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    [.,.,.,.,.,.] ]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

initialize(Board, 1) :- initBoard(Board).
% initialize([[.,.,.,.,.,.],[.,.,.,.,.,.],[.,.,1,2,.,.],[.,.,2,1,.,.],[.,.,.,.,.,.],[.,.,.,.,.,.]], 1).



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 

winner(Board, Winner) :-
    terminal(Board),
    append(Board, Flatten),
    atomic_list_concat(Flatten, "", Atom),
    atom_string(Atom, String),
    occurences(String, 1, Plyr1),
    occurences(String, 2, Plyr2),
    min(Plyr1, Plyr2, Winner).
    
occurences(Atom, Ch, N) :-
    aggregate_all(count, sub_atom(Atom, _,_,_, Ch), N).

min(X, Y, Z) :-
    (X >= Y
    -> Z = 1
    ;Z = 2).

%winner([[.,.,.,.,.,.],[.,.,.,.,.,.],[.,.,1,2,.,.],[.,.,2,1,.,.],[.,.,.,.,.,.],[.,.,.,.,.,1]], R).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(Board) :- 
    terminal(Board),
    append(Board, Flatten),
    atomic_list_concat(Flatten, "", Atom),
    atom_string(Atom, String),
    occurences(String, 1, Plyr1),
    occurences(String, 2, Plyr2),
    Plyr1 = Plyr2.

%tie([[.,.,.,.,.,.],[.,.,.,.,.,.],[.,.,1,2,.,.],[.,.,2,1,.,.],[.,.,.,.,.,.],[.,.,.,.,.,1]]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal, (neither players can make a move)

terminal(State) :-
    moves(1, State, M1),
    moves(2, State, M2),
    M1 == [n],
    M2 == [n].



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%

% initialize(B, 1), showState(B), moves(1, B, R), length(R, L).
moves(Plyr, State, MvList) :- 
    findall([X, Y], validmove(Plyr, State, [X, Y]), MvList).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%

nextState(Plyr, Move, State, NewState, NextPlyr) :-
    validmove(Plyr, State, Move),
    set(State, NextState, Move, Plyr) -> 
    nw(Move, NW), nw_flip(Plyr, NextState, NW, StateNW),
    nn(Move, NN), nn_flip(Plyr, StateNW, NN,  StateNN),
    ne(Move, NE), ne_flip(Plyr, StateNN, NE, StateNE),
    ww(Move, WW), ww_flip(Plyr, StateNE, WW, StateWW),
    ee(Move, EE), ee_flip(Plyr, StateWW, EE, StateEE),
    sw(Move, SW), sw_flip(Plyr, StateEE, SW, StateSW),
    ss(Move, SS), ss_flip(Plyr, StateSW, SS, StateSS),
    se(Move, SE), se_flip(Plyr, StateSS, SE, NewState),
    nextPlayer(Plyr, NextPlyr).

nextPlayer(1,2).
nextPlayer(2,1).

nw_flip(Plyr, Board, Proposed, NewBoard) :- 
    pos_player(Plyr, Board, Proposed),
    NewBoard = Board.
nw_flip(Plyr, Board, Proposed, NewBoard) :- 
    pos_opponent(Plyr, Board, Proposed) -> 
        set(Board, NextBoard, Proposed, Plyr),
        nw(Proposed, NW),
        nw_flip(Plyr, NextBoard, NW, NewBoard);
	NewBoard = Board.

nn_flip(Plyr, Board, Proposed, NewBoard) :- 
    pos_player(Plyr, Board, Proposed),
    NewBoard = Board.
nn_flip(Plyr, Board, Proposed, NewBoard) :- 
    pos_opponent(Plyr, Board, Proposed) -> 
        set(Board, NextBoard, Proposed, Plyr),
        nn(Proposed, NN),
        nn_flip(Plyr, NextBoard, NN, NewBoard);
	NewBoard = Board.

ne_flip(Plyr, Board, Proposed, NewBoard) :- 
    pos_player(Plyr, Board, Proposed),
    NewBoard = Board.
ne_flip(Plyr, Board, Proposed, NewBoard) :- 
    pos_opponent(Plyr, Board, Proposed) -> 
        set(Board, NextBoard, Proposed, Plyr),
        ne(Proposed, NE),
        ne_flip(Plyr, NextBoard, NE, NewBoard);
	NewBoard = Board.

ww_flip(Plyr, Board, Proposed, NewBoard) :- 
    pos_player(Plyr, Board, Proposed),
    NewBoard = Board.
ww_flip(Plyr, Board, Proposed, NewBoard) :- 
    pos_opponent(Plyr, Board, Proposed) -> 
        set(Board, NextBoard, Proposed, Plyr),
        ww(Proposed, WW),
        ww_flip(Plyr, NextBoard, WW, NewBoard);
	NewBoard = Board.

ee_flip(Plyr, Board, Proposed, NewBoard) :- 
    pos_player(Plyr, Board, Proposed),
    NewBoard = Board.
ee_flip(Plyr, Board, Proposed, NewBoard) :- 
    pos_opponent(Plyr, Board, Proposed) -> 
        set(Board, NextBoard, Proposed, Plyr),
        ee(Proposed, EE),
        ee_flip(Plyr, NextBoard, EE, NewBoard);
	NewBoard = Board.

sw_flip(Plyr, Board, Proposed, NewBoard) :- 
    pos_player(Plyr, Board, Proposed),
    NewBoard = Board.
sw_flip(Plyr, Board, Proposed, NewBoard) :- 
    pos_opponent(Plyr, Board, Proposed) -> 
        set(Board, NextBoard, Proposed, Plyr),
        sw(Proposed, SW),
        sw_flip(Plyr, NextBoard, SW, NewBoard);
	NewBoard = Board.

ss_flip(Plyr, Board, Proposed, NewBoard) :- 
    pos_player(Plyr, Board, Proposed),
    NewBoard = Board.
ss_flip(Plyr, Board, Proposed, NewBoard) :- 
    pos_opponent(Plyr, Board, Proposed) -> 
        set(Board, NextBoard, Proposed, Plyr),
        ss(Proposed, SS),
        ss_flip(Plyr, NextBoard, SS, NewBoard);
	NewBoard = Board.

se_flip(Plyr, Board, Proposed, NewBoard) :- 
    pos_player(Plyr, Board, Proposed),
    NewBoard = Board.
se_flip(Plyr, Board, Proposed, NewBoard) :- 
    pos_opponent(Plyr, Board, Proposed) -> 
        set(Board, NextBoard, Proposed, Plyr),
        se(Proposed, SE),
        se_flip(Plyr, NextBoard, SE, NewBoard);
	NewBoard = Board.

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.

% Check if move is valid
validmove(Plyr, Board, Proposed) :- 
    pos_empty(Board, Proposed),
    (nw(Proposed, NW), pos_opponent(Plyr, Board, NW)
    -> nw_valid(Plyr, Board, NW)).
    
validmove(Plyr, Board, Proposed) :- 
    pos_empty(Board, Proposed),
    (nn(Proposed, NN), pos_opponent(Plyr, Board, NN)
    -> nn_valid(Plyr, Board, NN)).

validmove(Plyr, Board, Proposed) :- 
    pos_empty(Board, Proposed),
    (ne(Proposed, NE), pos_opponent(Plyr, Board, NE)
    -> ne_valid(Plyr, Board, NE)).

validmove(Plyr, Board, Proposed) :- 
    pos_empty(Board, Proposed),
    (ww(Proposed, WW), pos_opponent(Plyr, Board, WW)
    -> ww_valid(Plyr, Board, WW)).

validmove(Plyr, Board, Proposed) :- 
    pos_empty(Board, Proposed),
    (ee(Proposed, EE), pos_opponent(Plyr, Board, EE)
    -> ee_valid(Plyr, Board, EE)).

validmove(Plyr, Board, Proposed) :- 
    pos_empty(Board, Proposed),
    (sw(Proposed, SW), pos_opponent(Plyr, Board, SW) 
    -> sw_valid(Plyr, Board, SW)).

validmove(Plyr, Board, Proposed) :- 
    pos_empty(Board, Proposed),
    (ss(Proposed, SS), pos_opponent(Plyr, Board, SS) 
    -> ss_valid(Plyr, Board, SS)).

validmove(Plyr, Board, Proposed) :- 
    pos_empty(Board, Proposed),
    (se(Proposed, SE), pos_opponent(Plyr, Board, SE) 
    -> se_valid(Plyr, Board, SE)).

% Check if pos has players stone
pos_player(Plyr, Board, [X, Y]) :- 
    get(Board, [X, Y], Value),
    Value == Plyr.

% Check if pos has opponents stone
pos_opponent(Plyr, Board, [X, Y]) :- 
    get(Board, [X, Y], Value),
    (Plyr = 1 -> Value == 2 ;Value == 1).

% Check if pos is empty
pos_empty(Board, [X, Y]) :- 
    get(Board, [X, Y], Value),
    Value == '.'.

% Check valid directions
nw_valid(Plyr, Board, Proposed) :- pos_player(Plyr, Board, Proposed).
nw_valid(Plyr, Board, Proposed) :- 
    pos_opponent(Plyr, Board, Proposed),
    nw(Proposed, NW),
    nw_valid(Plyr, Board, NW).

nn_valid(Plyr, Board, Proposed) :- pos_player(Plyr, Board, Proposed).
nn_valid(Plyr, Board, Proposed) :- 
    pos_opponent(Plyr, Board, Proposed),
    nn(Proposed, NN),
    nn_valid(Plyr, Board, NN).

ne_valid(Plyr, Board, Proposed) :- pos_player(Plyr, Board, Proposed).
ne_valid(Plyr, Board, Proposed) :- 
    pos_opponent(Plyr, Board, Proposed),
    ne(Proposed, NE),
    ne_valid(Plyr, Board, NE).

ww_valid(Plyr, Board, Proposed) :- pos_player(Plyr, Board, Proposed).
ww_valid(Plyr, Board, Proposed) :- 
    pos_opponent(Plyr, Board, Proposed),
    ww(Proposed, WW),
    ww_valid(Plyr, Board, WW).

ee_valid(Plyr, Board, Proposed) :- pos_player(Plyr, Board, Proposed).
ee_valid(Plyr, Board, Proposed) :- 
    pos_opponent(Plyr, Board, Proposed),
    ee(Proposed, EE),
    ee_valid(Plyr, Board, EE).

sw_valid(Plyr, Board, Proposed) :- pos_player(Plyr, Board, Proposed).
sw_valid(Plyr, Board, Proposed) :- 
    pos_opponent(Plyr, Board, Proposed),
    sw(Proposed, SW),
    sw_valid(Plyr, Board, SW).

ss_valid(Plyr, Board, Proposed) :- pos_player(Plyr, Board, Proposed).
ss_valid(Plyr, Board, Proposed) :- 
    pos_opponent(Plyr, Board, Proposed),
    ss(Proposed, SS),
    ss_valid(Plyr, Board, SS).

se_valid(Plyr, Board, Proposed) :- pos_player(Plyr, Board, Proposed).
se_valid(Plyr, Board, Proposed) :- 
    pos_opponent(Plyr, Board, Proposed),
    se(Proposed, SE),
    se_valid(Plyr, Board, SE).

% locations for 8 winds direction
nw([X, Y], [NW_X, NW_Y]) :- 
    NW_X is X - 1,
    NW_X >= 0,
    NW_Y is Y - 1,
    NW_Y >= 0.

nn([X, Y], [NN_X, NN_Y]) :- 
    NN_X is X + 0,
    NN_X >= 0,
    NN_Y is Y - 1,
    NN_Y >= 0.

ne([X, Y],[NE_X, NE_Y]) :- 
    NE_X is X + 1,
    NE_X < 6,
    NE_Y is Y - 1,
    NE_Y >= 0.

ww([X, Y], [WW_X, WW_Y]) :-
    WW_X is X - 1,
    WW_X >= 0,
    WW_Y is Y + 0,
    WW_Y >= 0.

ee([X, Y], [EE_X, EE_Y]) :-
    EE_X is X + 1,
    EE_X < 6,
    EE_Y is Y + 0,
    EE_Y >= 0.

sw([X, Y], [SW_X, SW_Y]) :- 
    SW_X is X - 1,
    SW_X >= 0,
    SW_Y is Y + 1,
    SW_Y < 6.

ss([X, Y], [SS_X, SS_Y]) :- 
    SS_Y is Y + 1,
    SS_Y < 6,
    SS_X is X + 0,
    SS_X >= 0.

se([X, Y], [SE_X, SE_Y]) :- 
    SE_X is X + 1,
    SE_X < 6,
    SE_Y is Y + 1,
    SE_Y < 6.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
