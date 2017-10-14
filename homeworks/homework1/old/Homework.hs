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
factorial n | n < 0 = error "Factorial of a negative number"
            | otherwise = product [1..n]

-- c)
maclaurin :: [Double]
maclaurin = [1 / factorial n | n <- [0..]]

-- d)
exp' :: Double -> Double
expWithN x n = evaluate x $ take n maclaurin
exp' x = expWithN x 170

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

-- TASK 4 ----------------------------------------


retainLettersAndSpaces s = [toLower c | c <- s, isLetter c || isSpace c]

wordFreq :: String -> [(String, Double)]
wordFreq s = wordFreq' (retainLettersAndSpaces s) []

addToWordFreq w acc = if contains acc w then update acc w $ (lookup' acc w) + 1 else insert' acc (w, 1)
wordFreq' s acc | null $ ws = acc
                | otherwise = wordFreq' (unwords $ tail ws) $ addToWordFreq (head ws) acc 
                where ws = words s


keysInBoth :: [(String, a)] -> [(String, a)] -> [String]
keysInBoth f1 f2 = sort $ nub [fst x1 | x1 <- f1, x2 <- f2, fst x1 == fst x2]



valueVectorWithKeys freqs keys = [lookup' freqs key | key <- keys] 
valueVector freqs = [snd freq | freq <- freqs]

cosineSimilarity :: String -> String -> Double
cosineSimilarity s1 s2 = num / den
                         where freqs1 = wordFreq s1
                               freqs2 = wordFreq s2
                               keys = keysInBoth freqs1 freqs2
                               commonVec1 = valueVectorWithKeys freqs1 keys 
                               commonVec2 = valueVectorWithKeys freqs2 keys
                               num = sum [fst v * snd v | v <- zip commonVec1 commonVec2]
                               vec1 = valueVector freqs1
                               vec2 = valueVector freqs2
                               den1 = sqrt $ sum [v * v | v <- vec1]
                               den2 = sqrt $ sum [v * v | v <- vec2]
                               den = den1 * den2


