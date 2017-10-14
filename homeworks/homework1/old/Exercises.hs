import Data.Char
import Data.List

--- Lecture 1

-- Exercise 1
concat3 s1 s2 s3 | length s2 < 2 = s1 ++ s3
                 | otherwise = s1 ++ s2 ++ s3

showSalary amount bonus | amount < 0 = "Salary is negative"
                        | otherwise = "Salary is " ++ show amount ++ (if bonus /= 0 then ", and a bonus " ++ show bonus else "")
--

-- Lecture 2
--
-- Exercise 1
middleOfList xs = drop 3 $ take (length xs - 3) xs

initials s1 s2 = (toUpper $ head s1) : ". " ++ (toUpper $ head s2) : "."

concatLongerFirst s1 s2 | length s1 >= length s2 = s1 ++ s2
                        | otherwise = concatLongerFirst s2 s1

safeHead l | null l = []
           | otherwise = [head l]

hasDuplicates xs = (length xs) == (length $ nub xs)
--

-- Exercise 2
doublesFromTo a b | a < b = [x*2 | x <- [a..b]]
                  | otherwise = doublesFromTo b a

caesarRange = ['a'..'z']
caesarCode n xs = [toLower $ head $ (drop (ord c - ord 'a' + n) $ cycle caesarRange) | c <- xs, c /= ' ']

--

-- Exercise 3
letterCount s = sum [length w | w <- words s, length w >= 3]

removeWhiteSpacesAndToLower s = [toLower c | c <- s, c /= ' ']
isPalindrome s = (removeWhiteSpacesAndToLower s) == (reverse $ removeWhiteSpacesAndToLower s)

isPalindrome' s | length s < 2 = True
                | head s == ' ' = isPalindrome' $ tail s
                | last s == ' ' = isPalindrome' $ init s
                | (toLower $ head s) == (toLower $ last s) = isPalindrome' $ init $ tail s
                | otherwise = False

flipp xss = concat $ [reverse xs | xs <- reverse xss]
--

-- Exercise 4
inCircle r x y = [(a, b) | a <- [-10..10], b <- [-10..10], (a - x)^2 + (b - y)^2 <= r^2]
inCircleGrid r x y gridX gridY = [(a, b) | a <- gridX, b <- gridY, (a - x)^2 + (b - y)^2 <= r^2]

inCircle' r x y = inCircleGrid r x y [-10..10] [-10..10]

steps xs = zip xs $ tail xs
--

-- Exercise 5
indices x xs = [fst ix | ix <- zip [0..] xs, snd ix == x]

showLineNumbers s = unlines [(show $ fst ix) ++ " " ++ snd ix | ix <- zip [1..] $ lines s]

haveAlignment xs ys = or [fst ix == snd ix | ix <- zip xs ys]
common xs ys = [fst ix | ix <- zip xs ys, fst ix == snd ix]




