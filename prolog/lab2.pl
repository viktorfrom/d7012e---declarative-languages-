

% -- Generate subsets subroutine, left to right
% subroutine_subsets :: [Int] -> Int -> Int -> [(Int, Int, Int, [Int])]
% subroutine_subsets xs i j
%   | length xs==0 = []
%   | otherwise = (sum xs, i, j, xs) : subroutine_subsets (init xs) i (j - 1)

% -- Generate subsets
% -- Generate subsets subroutine, left to right
% subroutine_subsets :: [Int] -> Int -> Int -> [(Int, Int, Int, [Int])]
% subroutine_subsets xs i j
%   | length xs==0 = []
%   | otherwise = (sum xs, i, j, xs) : subroutine_subsets (init xs) i (j - 1)

% -- Generate subsets
% generate_subsets :: [Int] -> Int -> Int -> [(Int, Int, Int, [Int])]
% generate_subsets xs i j
%   | length xs==0 = []
%   | otherwise = subroutine_subsets xs i j ++ generate_subsets (tail xs) (i + 1) j


subroutine_subsets([], 0).
subroutine_subsets(XS, I, J, L) :-
    sum_list(XS, Sum_XS),
    L = (Sum_XS, I, J, XS).

% Generate subsets
generate_subsets([], 0).
generate_subsets(XS, L) :-
    head(XS, Res), 
    tail(XS, Res2), 
    append([Res], Res2, L).
sda
sd
sum_list([], 0).
sum_list([Head|Tail], Sum) :-
   sum_list(Tail, Rest),
   Sum is Head + Rest.

head([X|_], X).

tail([_|X], X).