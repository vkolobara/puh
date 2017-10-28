import Data.Char
import Data.List


-- Corecursion
--



-- Accumulators
--


-- Exercise 1 -----------------------------------------------
length' :: [a] -> Int
length' xs = len xs 0
  where len [] n     = n
        len (_:xs) n = let acc = 1+n in acc `seq` len xs acc -- Strictness

maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip [] = error "Empty list"
maxUnzip ((a, b):xs) = maxUnzip' xs a b 
  where maxUnzip' [] maxA maxB          = (maxA, maxB)
        maxUnzip' ((a, b):xs) maxA maxB = (max a maxA, max b maxB)

maxUnzip'' :: [(Int, Int)] -> (Int, Int)
maxUnzip'' []          = error "empty list"
maxUnzip'' ((a, b):[]) = (a, b)
maxUnzip'' ((a, b):xs) = (max a maxA, max b maxB)
  where (maxA, maxB) = maxUnzip'' xs
------------------------------------------------------------


-- Guarded recursion

-- Strictness
