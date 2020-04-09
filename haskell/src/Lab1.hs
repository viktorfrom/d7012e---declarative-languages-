module Lab1
    ( smallest_k_sets
    ) where

smallest_k_sets :: [(Int, Int, Int, [Int])]
smallest_k_sets = subsets list

list :: [Int]
list = [-1, 2, -3, 4, -5]
--list = [-1]

subsets :: [(Int, Int, Int, [Int])] -> [(Int, Int, Int, [Int])]
subsets list
  | length list==1 = [(sum list, 1, length list, list)] 
  | otherwise = subsets [(sum list, head list, length list - 1, list)]


merge_sort :: Ord a => [a] -> [a]
merge_sort [] = []
merge_sort xs = go [[x] | x <- xs]
    where
    go [a] = a
    go xs = go (pairs xs)
    pairs (a:b:t) = merge a b : pairs t
    pairs t = t

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys