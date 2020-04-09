module Lab1
    ( smallest_k_sets
    ) where

smallest_k_sets :: (Integral a) => [a]
smallest_k_sets = result

list :: (Integral a) => [a]
list = [-1, 2, -3, 4, -5]

result :: (Integral a) => [a]
result = msort list

msort :: Ord a => [a] -> [a]
msort [] = []
msort xs = go [[x] | x <- xs]
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