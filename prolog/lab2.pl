

% -- Generate subsets subroutine, left to right
% subroutine_subsets :: [Int] -> Int -> Int -> [(Int, Int, Int, [Int])]
% subroutine_subsets xs i j
%   | length xs==0 = []
%   | otherwise = (sum xs, i, j, xs) : subroutine_subsets (init xs) i (j - 1)

% -- Generate subsets
% Generate subsets
generate_subsets(XS, L) :-
    head(XS, Res), tail(XS, Res2), append([Res], Res2, L).

head([X|_], X).

tail([_|X], X).