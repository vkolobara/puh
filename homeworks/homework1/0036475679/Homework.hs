module Homework where
--
import Data.List
import Data.Char
--


-- Task 01
isLeapYear :: Int -> Bool
isLeapYear y | mod y 400 == 0 = True
             | mod y 100 == 0 = False
             | mod y 4 == 0   = True
             | otherwise      = False

leapList :: [Int]
leapList = [y | y <- [1996..2017], isLeapYear y]

-- Task 02
evaluate :: Double -> [Double] -> Double
evaluate x coeffs = sum [fst p * snd p | p <- zip coeffs $ expList x (length coeffs)]

-- Get x^i for [0..n]
expList :: Double -> Int -> [Double]
expList x n = [x^k | k <- [0..n]]

factorial :: Double -> Double
factorial n | n < 0     = error "Factorial of a negative number"
            | otherwise = product [1..n]

maclaurin :: [Double]
maclaurin = [1 / factorial n | n <- [0..]]

exp' :: Double -> Double
expWithN x n = evaluate x $ take n maclaurin
exp' x = expWithN x 170

-- Task 03
findItem :: [(String, a)] -> String -> [(String, a)]
findItem xs key = take 1 [x | x <- xs, fst x == key]

contains :: [(String, a)] -> String -> Bool
contains xs key = not $ null $ findItem xs key

lookup :: [(String, a)] -> String -> a
lookup xs key | contains xs key = snd $ head $ findItem xs key
              | otherwise       = error "Key doesn't exist!"

insert :: [(String, a)] -> (String, a) -> [(String, a)]
insert xs item | contains xs $ fst item = xs
               | otherwise              = item : xs

remove :: [(String, a)] -> String -> [(String, a)]
remove xs key = [item | item <- xs, fst item /= key]

update :: [(String, a)] -> String -> a -> [(String, a)]
update xs key value | contains xs key = Homework.insert (remove xs key) (key, value) 
                    | otherwise       = xs

-- Task 04
-- Converts all letters to lower and removes everything that is not a letter or whitespace
retainLettersAndSpaces :: String -> String
retainLettersAndSpaces s = [toLower c | c <- s, isLetter c || isSpace c]

-- Calculates word frequencies for a string
wordFreq :: String -> [(String, Double)]
wordFreq s = wordFreq' (retainLettersAndSpaces s) []

-- Adds a word to word frequency list
addToWordFreq :: String -> [(String, Double)] -> [(String, Double)]
addToWordFreq w acc | contains acc w = update acc w $ (Homework.lookup acc w) + 1
                    | otherwise      = Homework.insert acc (w, 1)

-- Really calculates frequencies by using an accumulator
wordFreq' :: String -> [(String, Double)] -> [(String, Double)]
wordFreq' s acc | null ws   = acc
                | otherwise = wordFreq' (unwords $ tail ws) $ addToWordFreq (head ws) acc
                where ws = words s

-- Gets the common keys for 2 frequency lists and sorts them
-- A bit inefficient since it compares every item from f1 with every item from f2
commonKeys :: [(String, a)] -> [(String, a)] -> [String]
commonKeys f1 f2 = sort $ nub [fst x1 | x1 <- f1, x2 <- f2, fst x1 == fst x2]

-- Returns a vector of values of the frequency list for the keys provided only
valueVectorWithKeys :: [(String, a)] -> [String] -> [a]
valueVectorWithKeys freqs keys = [Homework.lookup freqs key | key <- keys]

-- Returns a vector of values of the frequency list
valueVector :: [(String, a)] -> [a]
valueVector freqs = [snd freq | freq <- freqs]

-- Returns the norm of a vector
normOfVector vec = sqrt $ sum [ v*v | v<-vec]

cosineSimilarity :: String -> String -> Double
cosineSimilarity s1 s2 = num / den
  where freqs1 = wordFreq s1 -- Word frequencies for the first string
        freqs2 = wordFreq s2 -- Word frequencies for the second string
        keys   = commonKeys freqs1 freqs2 -- Get common keys for numerator
        cVec1  = valueVectorWithKeys freqs1 keys -- Vector of only the common keys
        cVec2  = valueVectorWithKeys freqs2 keys -- -//-
        num    = sum [fst v * snd v | v <- zip cVec1 cVec2] -- Calculate numerator
        v1     = valueVector freqs1 -- Vector of the first frequencies
        v2     = valueVector freqs2 -- Vector of the second frequnecies
        den    = normOfVector v1 * normOfVector v2 -- Calculate denumerator
