module Homework where
--
import Data.List
import Data.Char
import Data.Function ( fix )
--

-- Task 01

-- non accumulator style
factorial :: (Num a, Eq a) => a -> a
factorial = fix (\rec x -> if x == 0 then 1 else x * rec (x-1))

-- non accumulator style
sum' :: Num a => [a] -> a
sum' = fix (\rec xs -> if null xs then 0 else head xs + rec (tail xs))

-- accumulator style
factorial' :: (Num a, Eq a) => a -> a
factorial' = fix (\rec a x -> if x == 0 then a else rec (a*x) (x-1)) 1

-- accumulator style
sum'' :: Num a => [a] -> a
sum'' = fix (\rec a xs -> if null xs then a else rec (head xs + a) (tail xs)) 0

nats :: [Integer]
nats = fix (\rec x -> x:rec (succ x)) 1

map' :: (a -> b) -> [a] -> [b]
map' f = fix (\rec xs -> if null xs then [] else (f $ head xs):rec (tail xs))

zip' :: [a] -> [b] -> [(a, b)]
zip' = fix (\rec xs ys -> if null xs || null ys then [] else (head xs, head ys):rec (tail xs) (tail ys))

-- Task 02
subsets :: Eq a => Int -> [a] -> [[a]]
subsets n xs
  | n < 0     = error "n is negative"
  | otherwise = subsets' n $ nub xs

subsets' :: Eq a => Int -> [a] -> [[a]]
subsets' 0 _      = [[]]
subsets' n []     = []
subsets' n (x:xs) = [x:p | p <- subsets' (n-1) xs]
                    ++ [p | p <- subsets' n xs]


partitions :: Eq a => [a] -> [[[a]]]
partitions []     = error "partitioning empty set"
partitions xs     = partitions' xs

partitions' :: Eq a => [a] -> [[[a]]]
partitions' []     = [[]]
partitions' (x:xs) = [([x]:ps) | ps <- parts] ++ [ [(x:p)]++(delete p ps) | ps <- parts, p <- ps]
  where parts = partitions' xs

-- Task 03
permutations' :: Eq a => [a] -> [[a]]
permutations' []     = [[]]
permutations' xs = [ x:perms | x <- xs, perms <- permutations' $ delete x xs]
