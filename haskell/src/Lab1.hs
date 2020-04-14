module Lab1
    ( smallest_k_sets
    ) where

smallest_k_sets :: [Int]
smallest_k_sets
  | length list==0 = error("Invalid input: list is empty")
  | otherwise = quick_sort list

list :: [Int]
list = [-1, 2, -3, 4, -5]
--list = [-1]
--list = []

-- subsets :: [(Int, Int, Int, [Int])] -> [(Int, Int, Int, [Int])]
-- subsets list
--   | length list==1 = [(sum list, 1, length list, list)] 
--   | otherwise = subsets [(sum list, head list, length list - 1, list)]

quick_sort :: Ord a => [a] -> [a]
quick_sort [] = []
quick_sort (x:xs) = quick_sort less ++ equal ++ quick_sort larger
  where
    less   = [y | y<-xs, y<x]
    equal  = [y | y<-xs, y==x] ++ [x]
    larger = [y | y<-xs, y>x]