import Data.Char
import Data.List

--RECURSION
--
--

-- Exercise 1

product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs

headsOf :: [[a]] -> [a]
headsOf []            = []
headsOf ((x:_): xss)  = x : headsOf xss
headsOf (_:xss)       = headsOf xss


-- Exercise 2

modMult _ _ [] = []
modMult n m (x:xs) = x * (n `mod` m) : modMult n m xs

addPredecessor xs = pred 0 xs 
  where pred _ []     = []
        pred p (x:xs) = x+p : pred x xs


-- Exercise 3

equalTriplets :: Eq a => [(a, a, a)] -> [(a, a, a)]
equalTriplets []             = []
equalTriplets ((a, b, c):xs) | a == b && b == c = (a, b, c) : equalTriplets xs
                             | otherwise        = equalTriplets xs

replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' 1 x = [x]
replicate' n x = [x] ++ replicate' (n-1) x


-- Exercise 4

drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []     = []
drop' n (x:xs) = drop' (n-1) xs

drop'' :: Int -> [a] -> [a]
drop'' n xs | n<0       = reverse (drop' (-n) (reverse xs)) 
            | otherwise = drop' n xs 

takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo _ _ []      = []
takeFromTo _ 0 (x:_)   = [x]
takeFromTo 0 n2 (x:xs) = x : takeFromTo 0 (n2-1) xs
takeFromTo n1 n2 xs    = takeFromTo (n1-1) (n2-1) xs

--Exercise 5

eachThird :: [a] -> [a]
eachThird xs = eachThird' 1 xs
  where eachThird' _ [] = []
        eachThird' i (x:xs) | i `mod` 3 == 0 = x : eachThird' i+1 xs
                            | otherwise      = eachThird' i+1 xs

crossZip :: [a] -> [b] -> [(a, b)]
crossZip [] _                  = []
crossZip _ []                  = []
crossZip (x1:x2:xs) (y1:y2:ys) = [(x1, y2), (x2, y1)] : crossZip xs ys















