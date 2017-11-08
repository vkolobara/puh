module Homework where
--
import Data.List
import Data.Char
import Data.Function ( fix )
--

-- Task 01

-- non accumulator style
factorial :: (Num a, Eq a) => a -> a
factorial = undefined

-- non accumulator style
sum' :: Num a => [a] -> a
sum' = undefined

-- accumulator style
factorial' :: (Num a, Eq a) => a -> a
factorial' = undefined

-- accumulator style
sum'' :: Num a => [a] -> a
sum'' = undefined

nats :: [Integer]
nats = undefined

map' :: (a -> b) -> [a] -> [b]
map' = undefined

zip' :: [a] -> [b] -> [(a, b)]
zip' = undefined

-- Task 02
subsets :: Int -> [a] -> [[a]]
subsets = undefined

partitions :: [a] -> [[[a]]]
partitions = undefined

-- Task 03
permutations' :: [a] -> [[a]]
permutations' = undefined