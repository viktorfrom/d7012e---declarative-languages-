module Lab1
    ( smallest_k_sets
    ) where

smallest_k_sets :: [(Int, Int, Int, [Int])]
smallest_k_sets = generate_subsets (check_length list3) 1 (length list3)

-- Generate subsets subroutine, left to right
subroutine_subsets :: [Int] -> Int -> Int -> [(Int, Int, Int, [Int])]
subroutine_subsets xs i j
  | length xs==0 = []
  | otherwise = (sum xs, i, j, xs) : subroutine_subsets (init xs) i (j - 1)

-- Generate subsets
generate_subsets :: [Int] -> Int -> Int -> [(Int, Int, Int, [Int])]
generate_subsets xs i j
  | length xs==0 = []
  | otherwise = subroutine_subsets xs i j ++ generate_subsets (tail xs) (i + 1) j

check_length :: [Int] -> [Int]
check_length xs
  | length xs==0 = error("Invalid input: list is empty!")
  | otherwise = xs

quick_sort :: [(Int, Int, Int, [Int])] ->  [(Int, Int, Int, [Int])]
quick_sort [] = []
quick_sort (x : xs) = quick_sort less ++ equal ++ quick_sort larger
  where
    less   = [y | y<-xs, y<x ]
    equal  = [y | y<-xs, y==x] ++ [x]
    larger = [y | y<-xs, y>x ]


list3 :: [Int] 
list3 = [-1, 2, -3, 4, -5]

list2 :: [Int] 
list2 = [-1]

list1 :: [Int] 
list1 = []