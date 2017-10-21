{-# LANGUAGE NoMonomorphismRestriction #-}

--
module Exercises where
--
import           Data.Char
import           Data.List
--

{-
    Here you should provide your solutions to in-class exercises.

    Make sure that ALL FUNCTIONS (including exXXX) have correct TYPE SIGNATURES.

    You should include solutions from following lectures :
    - http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-04.lhs
    - http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-05.lhs

    DON'T change function names, just remove 'undefined' and write your own
    definition for that function.
-}

{-LECTURE 04-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-04.lhs

-- EXERCISE 01 =======================================================================

-- Define 'headHunter xss' that takes the head of the first list element. If
-- the first element has no head, it takes the head of the second element.
-- If the second element has no head, it takes the head of the third element.
-- If none of this works, the function returns an error.
ex411 :: [[a]] -> a
ex411 = headHunter
headHunter ::[[a]] -> a
headHunter ((x:_):_)     = x
headHunter (_:(x:_):_)   = x
headHunter (_:_:(x:_):_) = x
headHunter _             = error "error"

-- Define 'firstColumn m' that returns the first column of a matrix.
-- firstColumn [[1,2],[3,4]] => [1,3]
-- Check what happens if the input is not a valid matrix.
ex412 :: [[a]] -> [a]
ex412 = firstColumn
firstColumn :: [[a]] -> [a]
firstColumn m = [x | (x:_) <- m]

-- Define 'shoutOutLoud' that repeats three times the initial letter of each
-- word in a string.
-- shoutOutLoud :: String -> String
-- shoutOutLoud "Is anybody here?" => "IIIs aaanybody hhhere?"
ex413 :: String -> String
ex413 = shoutOutLoud
shoutOutLoud :: String -> String
shoutOutLoud s = unwords [replicate 3 c ++ xs | (c:xs) <- words s]

-- EXERCISE 02 =======================================================================

-- Define 'pad' that pads the shorter of two the strings with trailing spaces
-- and returns both strings capitalized.
-- pad :: String -> String -> (String, String)
-- pad "elephant" "cat" => ("Elephant", "Cat     ")
ex421 :: String -> String -> (String, String)
ex421 = pad
pad :: String -> String -> (String, String)
pad s1 s2 = (padOne $ capitalize s1, padOne $ capitalize s2)
  where capitalize (x:xs) = toUpper x : xs
        capitalize _      = ""
        l                 = length s1 `max` length s2
        padOne s          = take l (s ++ repeat ' ')

-- Define 'quartiles xs' that returns the quartiles (q1,q2,q3) of a given list.
-- The quartiles are elements at the first, second, and third quarter of a list
-- sorted in ascending order. (You can use the built-int 'splitAt' function and
-- the previously defined 'median' function.)
-- quartiles :: [Int] -> (Double,Double,Double)
-- quartiles [3,1,2,4,5,6,8,0,7] => (1.5, 4.0, 6.5)
median :: (Integral a, Fractional b) => [a] -> b
median [] = error "median: Empty list"
median xs
  | odd l     = realToFrac $ ys !! h
  | otherwise = realToFrac (ys !! h + ys !! (h-1)) / 2
  where l  = length xs
        h  = l `div` 2
        ys = sort xs

ex422 :: (Integral a, Fractional b) => [a] -> (b, b, b)
ex422 = quartiles
quartiles :: (Integral a, Fractional b) => [a] -> (b, b, b)
quartiles xs = (median part1, med, median part2)
  where len    = length xs
        sorted = sort xs
        med    = median xs
        half   = len `div` 2
        part1  = take half sorted
        part2  = drop (len-half) sorted


-- EXERCISE 03 =======================================================================

-- Redo Exercise 2 using 'let' instead of 'where'.
ex431 :: String -> String -> (String, String)
ex431 = pad'

pad' :: String -> String -> (String, String)
pad' s1 s2 =
  let capitalize (x:xs) = toUpper x : xs
      capitalize _      = ""
      l                 = length s1 `max` length s2
      padOne s             = take l (s ++ repeat ' ')
  in (padOne $ capitalize s1, padOne $ capitalize s2)

ex432 :: (Integral a, Fractional b) => [a] -> (b, b, b)
ex432 = quartiles'
quartiles' :: (Integral a, Fractional b) => [a] -> (b, b, b)
quartiles' xs =
    let len    = length xs
        sorted = sort xs
        med    = median xs
        half   = len `div` 2
        part1  = take half sorted
        part2  = drop (len-half) sorted
    in (median part1, med, median part2)

-- EXERCISE 04 =======================================================================

-- Write a function that takes in a pair (a,b) and a list [c] and returns the
-- following string:
-- "The pair [contains two ones|contains one one|does not contain a single one]
-- and the second element of the list is <x>"
ex441 :: Show b => (Int, Int) -> [b] -> String
ex441 = foo
foo :: Show b => (Int, Int) -> [b] -> String
foo (x, y) zs
  = "The pair " ++ case (x, y) of
    (1, 1) -> "contains two ones"
    (1, _) -> "contains one one"
    (_, 1) -> "contains one one"
    _      -> "does not contain a single one"
    ++ " and the second element of the list is " ++ case zs of
    (_:z:_) -> show z
    _       -> "nothing"


{-LECTURE 05-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-05.lhs

-- EXERCISE 01 =======================================================================

-- Define a recursive function to compute the product of a list of elements.
-- product' :: Num a => [a] -> a
ex511 :: Num a => [a] -> a
ex511 = product'
product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs

-- Define a recursive function 'headsOf' that takes a list of lists and
-- returns a list of their heads.
-- headsOf :: [[a]] -> [a]
-- headsOf [[1,2,3],[4,5],[6]] => [1,4,6]
ex512 :: [[a]] -> [a]
ex512 = headsOf
headsOf :: [[a]] -> [a]
headsOf []         = []
headsOf ([]:_)    = []
headsOf ((x:_):xs) = x : headsOf xs

-- EXERCISE 02 =======================================================================

-- Define a recursive function 'modMult n m xs' that multiplies each element of
-- a list 'xs' with 'n' modulo 'm'.
ex521 :: Integral a => a -> a -> [a] -> [a]
ex521 = modMult
modMult :: Integral a => a -> a -> [a] -> [a]
modMult _ 0 _  = error "Modulo with zero"
modMult n m xs = modMult' xs
  where modMult' []     = []
        modMult' (y:ys) = y * fac : modMult' ys
        fac                 = n `mod` m

-- Define a function 'addPredecessor' that adds to each element of a list the
-- value of the preceding element. The first element gets no value added.
-- addPredecessor :: Num a => [a] -> [a]
-- addPredecessor [3,2,1] => [3,5,3]
ex522 :: Num a => [a] -> [a]
ex522 = addPredecessor
addPredecessor :: Num a => [a] -> [a]
addPredecessor xs = accPred 0 xs
  where accPred _ []     = []
        accPred a (y:ys) = y+a : accPred y ys

-- EXERCISE 03 =======================================================================

-- Define 'equalTriplets' that filters from a list of triplets (x,y,z) all
-- triplets for which x==y==z.
-- equalTriplets [(1,2,3),(2,2,2),(4,5,6)] => [(2,2,2)]
ex531 :: Eq a => [(a, a, a)] -> [(a, a, a)]
ex531 = equalTriplets
equalTriplets :: Eq a => [(a, a, a)] -> [(a, a, a)]
equalTriplets []             = []
equalTriplets ((x, y, z):xs) | x == y && y == z = (x, y, z) : equalTriplets xs
                           | otherwise        = equalTriplets xs

-- Define your own version of the replicate function:
-- replicate' :: Int -> a -> [a]
ex532 :: Int -> a -> [a]
ex532 = replicate'
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n c | n < 0     = error "n less than 0"
               | otherwise = c : replicate' (n-1) c

-- EXERCISE 04 =======================================================================

-- Define your own recursive version of the drop function:
-- drop' :: Int -> [a] -> [a].
-- Define drop'' (a wrapper function) so that for n < 0 the function drops
-- the elements from the end of the list. You can use 'reverse'.
ex541 :: Int -> [a] -> [a]
ex541 = drop'
drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []     = []
drop' n (_:xs) = drop' (n-1) xs

ex541' :: Int -> [a] -> [a]
ex541' = drop''
drop'' :: Int -> [a] -> [a]
drop'' n xs | n < 0     = reverse $ drop' (-n) $ reverse xs
            | otherwise = drop' n xs

-- Define a recursive function 'takeFromTo n1 n2 xs'.
-- takeFromTo :: Int -> Int -> [a] -> [a]
ex542 :: Int -> Int -> [a] -> [a]
ex542 = takeFromTo
takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo n1 n2 xs | n1 < 0 || n2 < 0 = error "Indice less than 0"
                    | n2 < n1          = takeFromTo' n2 n1 xs
                    | otherwise        = takeFromTo' n1 n2 xs
  where takeFromTo' 0 e (y:ys) = y : takeFromTo' 0 (e-1) ys
        takeFromTo' _ 0 (x:_)   = [x]
        takeFromTo' _ _ []      = []
        takeFromTo' s e ys    = takeFromTo' (s-1) (e-1) ys
-- EXERCISE 05 =======================================================================

-- Define a recursive function 'eachThird' that retains every third element
-- in a list.
-- eachThird :: [a] -> [a]
-- eachThird "zagreb" => "gb"
ex551 :: [a] -> [a]
ex551 = eachThird
eachThird :: [a] -> [a]
eachThird xs = eachThird' 1 xs
  where eachThird' _ []     = []
        eachThird' 3 (y:ys) = y : eachThird' 1 ys
        eachThird' n (_:ys) = eachThird' (n+1) ys

-- Define a recursive function 'crossZip' that zips two lists in a "crossing"
-- manner:
-- crossZip [1,2,3,4,5] [4,5,6,7,8] => [(1,5),(2,4),(3,7),(4,6)]
ex552 :: [a] -> [b] -> [(a, b)]
ex552 = crossZip
crossZip :: [a] -> [b] -> [(a, b)]
crossZip (x1:x2:xs) (y1:y2:ys) = (x1, y2) : (x2, y1) : crossZip xs ys
crossZip _ _                   = []


-- EXERCISE 06 =======================================================================

-- Write an accumulator-style recursive definition of
-- length' :: [a] -> Int
ex561 :: [a] -> Int
ex561 = length'
length' :: [a] -> Int
length' xs = len 0 xs
  where len n []     = n
        len n (_:ys) = len (n+1) ys

-- Write an accumulator-style recursive definition of
--     maxUnzip :: [(Int, Int)] -> (Int, Int)
-- that returns the maximum element at the first position and the maximum
-- element at the second position in a pair, i.e., it's equivalent to:
--     maxUnzip zs = (maximum xs, maximum ys)
--         where (xs,ys) = unzip zs
-- If the list is empty, return an "empty list" error.
-- Now write a standard recursive definition maxUnzip' (without an accumulator).
ex562 :: [(Int, Int)] -> (Int, Int)
ex562 = maxUnzip
maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip []          = error "Max of empty list"
maxUnzip ((x, y):xs) = accMaxUnzip x y xs
  where accMaxUnzip m1 m2 []          = (m1, m2)
        accMaxUnzip m1 m2 ((a, b):ys) = accMaxUnzip (max a m1) (max b m2) ys

ex562' :: [(Int, Int)] -> (Int, Int)
ex562' = maxUnzip'
maxUnzip' :: [(Int, Int)] -> (Int, Int)
maxUnzip' []          = error "Max of empty list"
maxUnzip' ((x, y):[]) = (x, y)
maxUnzip' ((x, y):xs) = (max x maxX, max y maxY)
  where (maxX, maxY) = maxUnzip' xs
