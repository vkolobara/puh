import Data.Char
import Data.List

-- TASK 1 ----------------------------------------
isLeapYear :: Int -> Bool
isLeapYear y | mod y 400 == 0 = True
             | mod y 100 == 0 = False
             | mod y 4   == 0 = True
             | otherwise      = False

leapList :: [Int]
leapList = [y | y <- [1996..2017], isLeapYear y]
--------------------------------------------------

-- TASK 2 ----------------------------------------

-- a)
evaluate :: Double -> [Double] -> Double

expList x n = [x^k | k <- [0..n]]

evaluate x coeffs = sum [fst p * snd p | p <- zip coeffs $ expList x (length coeffs)]

-- b)
factorial :: Double -> Double
factorial n = product [1..n]

-- c)
maclaurin :: [Double]
maclaurin = [1 / factorial n | n <- [0..]]

-- d)
exp' :: Double -> Double
exp' x = evaluate x $ take 170 maclaurin

-- TASK 3 ----------------------------------------

-- a)
findItem :: [(String, a)] -> String -> [(String, a)]
findItem xs key = take 1 [x | x <- xs, fst x == key]

-- b)
contains :: [(String, a)] -> String -> Bool
contains xs key = not $ null $ findItem xs key

-- c)
lookup' :: [(String, a)] -> String -> a
lookup' xs key = if contains xs key then snd $ head $ findItem xs key else error "Key doesn't exist"

-- d)
insert' :: [(String, a)] -> (String, a) -> [(String, a)]
insert' xs item = if contains xs $ fst item then xs else item : xs

-- e)
remove :: [(String, a)] -> String -> [(String, a)]
remove xs key = [item | item <- xs, fst item /= key]

-- f)
update :: [(String, a)] -> String -> a -> [(String, a)]
update xs key value | contains xs key = (key, value) : remove xs key
                    | otherwise       = xs
