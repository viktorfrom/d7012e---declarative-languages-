module Lab1
    ( smallest_k_sets
    ) where



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

quick_sort :: [(Int, Int, Int, [Int])] -> [(Int, Int, Int, [Int])]
quick_sort [] = []
quick_sort (pivot : xs) = (quick_sort lesser) ++ [pivot] ++ (quick_sort greater)
    where
        lesser  = filter (< pivot) xs
        greater = filter (>= pivot) xs

check_length :: [Int] -> [Int]
check_length xs
  | length xs==0 = error("Invalid input: list is empty!")
  | otherwise = xs

result :: [Int] -> Int -> [(Int, Int, Int, [Int])]
result xs k
  | k <= 0 = error("Invalid input: second parameter needs to be >= 0!")
  | otherwise = take k (quick_sort(generate_subsets (check_length xs) 1 (length xs)))

smallest_k_sets :: [(Int, Int, Int, [Int])]
smallest_k_sets = result list3 3

list3 :: [Int]
list3 = [-1, 2, -3, 4, -5]

-- list2 :: [Int] 
-- list2 = [-1]

-- list1 :: [Int] 
-- list1 = []