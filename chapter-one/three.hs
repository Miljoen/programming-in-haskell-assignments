-- Define a function product that produces the product of a list of numbers,
-- and show using your definition that product [2,3,4] = 24.

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

-- Or

product'' :: Num a => [a] -> a
product'' = foldr (*) 1

-- Or

product''' :: Num a => [a] -> a
product''' = foldr1 (*)

-- ghci> product' [2,3,4]
-- 24
