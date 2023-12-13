-- What would be the effect of replacing <= by < in the original definition of qsort? 
-- Hint: consider the example qsort [2,2,3,1,1].

-- The original

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where smaller = [a | a <- xs, a <= x]
          larger = [b | b <- xs, b > x]

-- Adjusted

qsort' :: Ord a => [a] -> [a]
qsort' [] = []
qsort' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
    where smaller = [a | a <- xs, a < x]
          larger = [b | b <- xs, b > x]

-- qsort' [2,2,3,1,1]
-- applying qsort'
-- qsort' [1,1] ++ [2] ++ qsort' [3]
-- applying qsort'
-- [] ++ [1] ++ [] ++ [2] ++ [] ++ [3] ++ []
-- applying ++
-- [1,2,3]

-- The effect of replacing <= by < is filtering elements that are equal to a given x, this means duplicates are removed.
-- The result is a sorted list without any duplicates.
