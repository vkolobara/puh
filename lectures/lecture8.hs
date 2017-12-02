import Data.Char
import Prelude 
import Data.List 
import Control.Monad
import Data.Ord (comparing)

-- Composition

--EXERCISES
sumEven :: [Integer] -> Integer
sumEven = sum . map snd . filter (even . fst) . zip [0..]

--filterWords :: [String] -> String -> String
--filterWords ws s = filter () . words

maxDiff xs = maximum $ map (abs . uncurry (-)) $ zip xs $ tail xs

maxMinDiff xs = (minimum diff_list, maximum diff_list)
  where diff_list = map (abs . uncurry (-)) $ zip xs $ tail xs

studentsPassed :: [(String, Float)] -> [String]
studentsPassed xs = map fst . filter ((>=m) . snd) $ xs
  where m = maximum (map snd xs) / 2

isTitleCased = all (isUpper . head) . words

-- Exercise 4 ----------
elem' x = foldr (\y acc -> y == x || acc ) False
reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++ [x]) []

nubRuns :: Eq a => [a] -> [a]
nubRuns = foldr (\x acc -> if null acc || head acc /= x then x:acc else acc) []
-----------------------
--Foldl
-- Exercise 5 ------------
reverse'' :: [a] -> [a]
reverse'' = foldl flip(:) []

sumEven' :: [Integer] -> Integer
sumEven' = fst . foldl (\(curr, flag) x -> if flag then (curr+x, False) else (curr, True)  ) (0,True)

maxUnzip :: [(Int,Int)] -> (Int, Int)
maxUnzip = foldl1 (\(maxX, maxY) (x, y) -> (max x maxX, max y maxY))
---------------------------



