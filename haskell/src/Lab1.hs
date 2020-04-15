module Lab1
    ( smallest_k_sets
    ) where

smallest_k_sets :: [(Int, Int, [Int])]
smallest_k_sets = generate_subsets list3 1
  -- | length list3==0 = error("Invalid input: list is empty!")
  -- | otherwise = generate_subsets list3 1

-- Generate subsets right to left
generate_subsets1 :: [Int] -> Int -> [(Int, Int, [Int])]
generate_subsets1 xs i
  | length xs==0 = []
  | otherwise = (sum xs, i, xs) : generate_subsets1 (init xs) i

-- Generate subsets left to right
generate_subsets :: [Int] -> Int -> [(Int, Int, [Int])]
generate_subsets xs i
  | length xs==0 = []
  | otherwise = (sum xs, i, xs) : generate_subsets (tail xs) (i + 1)


quick_sort :: Ord a => [a] -> [a]
quick_sort [] = []
quick_sort (x:xs) = quick_sort less ++ equal ++ quick_sort larger
  where
    less   = [y | y<-xs, y<x]
    equal  = [y | y<-xs, y==x] ++ [x]
    larger = [y | y<-xs, y>x]


list3 :: [Int] 
list3 = [-1, 2, -3, 4, -5]

list2 :: [Int] 
list2 = [-1]

list1 :: [Int] 
list1 = []