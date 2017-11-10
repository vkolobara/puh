import Data.Char
import Prelude 
import Data.List 
import Control.Monad
import Data.Ord (comparing)

-- Composition

--EXERCISES
--sumEven :: [Integer] -> Integer
--sumEven = map snd . filter (even . fst) . zip [0..]


--filterWords :: [String] -> String -> String
--filterWords ws s = filter () . words



maxDiff xs = maximum $ map (abs . uncurry (-)) $ zip xs $ tail xs

maxMinDiff xs = (minimum diff_list, maximum diff_list)
  where diff_list = map (abs . uncurry (-)) $ zip xs $ tail xs

studentsPassed :: [(String, Float)] -> [String]
studentsPassed xs = map fst . filter ((>=m) . snd) $ xs
  where m = maximum (map snd xs) / 2

isTitleCased = all (isUpper . head) . words

elem' x = foldr (\y acc -> y == x || acc ) False
reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++ [x]) []

