import Data.Char
import Prelude hiding (map, filter)
import Data.List hiding (map, filter)

-- Partial applications


-- Sections


-- Exercise 1 -------------------------------

takeThree = take 3
dropThree = drop 3
hundredTimes = replicate 100


index = zip [0..]
index' xs = zip xs [0..]


divider n = replicate n '='
---------------------------------------------

-- Higher-order functions


-- Exercise 2 -------------------------------
applyOnLast f xs ys = f $ last xs $ last ys
lastTwoPlus100 = undefined

applyManyTimes n f x
  | n <= 0    = x
  | otherwise = applyManyTimes (n-1) f (f x)

applyTwice = applyManyTimes 2
---------------------------------------------

-- MAP

map _ []     = []
map f (x:xs) = f x : map f xs

-- Exercise 3 -------------------------------

listifylist = map (:[])

cutoff n = map (min n)
---------------------------------------------

-- FILTER

filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

-- Exercise 4 -------------------------------

sumEvenSquares :: [Integer] -> Integer
sumEvenSquares xs = sum $ map (^2) $ filter (even) xs 

freq x xs = length $ filter (==x) xs

freqFilter n xs = undefined

---------------------------------------------

-- LAMBDA EXPRESSIONS

-- Exercise 5 -------------------------------

withinInterval n m = filter (\x -> x >= n && x <= m)

sndColumn = map (\r -> r !! 1)

canoinicializePairs xs = map (\(x, y) -> if x < y then (x, y) else (y, x)) $ filter (\(x, y) -> x /= y) xs
---------------------------------------------
