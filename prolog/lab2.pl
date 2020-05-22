% -- Generate subsets subroutine, left to right
% subroutine_subsets :: [Int] -> Int -> Int -> [(Int, Int, Int, [Int])]
% subroutine_subsets xs i j
%   | length xs==0 = []
%   | otherwise = (sum xs, i, j, xs) : subroutine_subsets (init xs) i (j - 1)

% Subroutine_subsets
subroutine_subsets([], _, _, []).
subroutine_subsets(XS, I, J, [ [Sum, I, J, XS] |Res]) :-
    sum_list(XS, Sum),
    init(XS, Init),
    New_J is J - 1,
    subroutine_subsets(Init, I, New_J, Res).

% -- Generate subsets
% generate_subsets :: [Int] -> Int -> Int -> [(Int, Int, Int, [Int])]
% generate_subsets xs i j
%   | length xs==0 = []
%   | otherwise = subroutine_subsets xs i j ++ generate_subsets (tail xs) (i + 1) j

% Generate subsets
generate_subsets([], _, _, []).
generate_subsets(XS, I, J, [X | Res]) :-
    tail(XS, Tail), 
    New_I is I + 1,
    subroutine_subsets(XS, I, J, X),
    generate_subsets(Tail, New_I, J, Res).

% Return list without head element
init([_], []).
init([Head | Tail], [Head | I]):-
    init(Tail, I).

% Return sum of list
sum_list([], 0).
sum_list([Head|Tail], Sum) :-
   sum_list(Tail, Rest),
   Sum is Head + Rest.

% result([], "Invalid input, list empty!").
% result(XS, Res) :-
%     length(XS, Len),
%     generate_subsets(XS, 1, Len, Res).

head([X|_], X).

tail([_|X], X).