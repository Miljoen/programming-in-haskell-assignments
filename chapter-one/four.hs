-- How should the definition of the function qsort be modified so that it produces a reverse sorted version of a list?


-- The original

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where smaller = [a | a <- xs, a <= x]
          larger = [b | b <- xs, b > x]

-- Adjusted

reverseQSort :: Ord a => [a] -> [a]
reverseQSort [] = []
reverseQSort (x:xs) = reverseQSort larger ++ [x] ++ reverseQSort smaller
    where larger = [a | a <- xs, a > x]
          smaller = [a | a <- xs, a <= x]

-- ghci> qsort [1,4,2,3,1,2,3]
-- [1,1,2,2,3,3,4]
-- ghci> reverseQSort [1,4,2,3,1,2,3]
-- [4,3,3,2,2,1,1]
