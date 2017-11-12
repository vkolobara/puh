module Homework where
--
import           Data.Char
import           Data.Function (fix)
import           Data.List
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
map' f = fix (\rec xs -> if null xs then [] else f (head xs):rec (tail xs))

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
subsets' n (x:xs) = map (x:) (subsets' (n-1) xs) -- Subsets with the current element
                    ++ subsets' n xs -- Subsets without the current element

partitions :: [a] -> [[[a]]]
partitions [] = error "partitioning empty set"
partitions xs = partitions'' xs

partitions'' :: [a] -> [[[a]]]
partitions'' []     = [[]]
partitions'' (x:xs) = map ([x]:) parts -- insert the element as a separate partition
                      ++ concatMap (mapWithRemainder x) parts -- insert the element into all partitions
  where parts = partitions'' xs

-- Maps current element to all the partitions, but retains the remaining partitions, e.g.:
-- mapWithRemainder 'a' ["bc", "d", ""] -> [["abc", "d", ""], ["bc", "ad", ""], ["bc", "d", "a"]]
mapWithRemainder :: a -> [[a]] -> [[[a]]]
mapWithRemainder e xs = [ take i xs ++ (e:xs!!i) : drop (i+1) xs | i <- [0..length xs-1]]

-- Faster implementation with Eq constraint
partitions' :: Eq a => [a] -> [[[a]]]
partitions' []     = [[]]
partitions' (x:xs) = map ([x]:) parts -- insert the element as a separate partition
                    ++ [ (x : p) : delete p ps | ps <- parts, p <- ps] -- insert the element into all partitions
  where parts =  partitions' xs

-- Task 03
permutations' :: [a] -> [[a]]
permutations' []     = [[]]
permutations' [x]    = [[x]]
permutations' (x:xs) = [ take i perm ++ x : drop i perm | perm <- perms, i <- [0..length perm]] -- insert the omitted element into all positions of the permutations of the remainder
  where perms = permutations' xs
