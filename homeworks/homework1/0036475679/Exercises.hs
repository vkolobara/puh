module Exercises where
--
import Data.List
import Data.Char
--

{-
    Here you should provide your solutions to in-class exercises.
    
    Make sure that ALL FUNCTIONS have correct TYPE SIGNATURES.
    
    You should include solutions from following lectures :
    - http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-01.lhs
    - http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-02.lhs
    - http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-03.lhs

    DON'T change function names, just remove 'undefined' and write your own
    definition for that function.
-}

{-LECTURE 01-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-01.lhs

-- EXERCISE 01========================================================================
ex111 :: String -> String -> String -> String
ex111 = concat3
concat3 s1 s2 s3 | length s2 < 2 = s1 ++ s3
                 | otherwise     = s1 ++ s2 ++ s3 

ex112 :: Double -> Double -> String
ex112 = showSalary
showSalary amount bonus | amount < 0 = "Amount is negative"
                        | otherwise  = "Salary is " ++ show amount ++ 
                                        (if bonus /= 0 then ", and a bonus " ++ show bonus else "")

{-LECTURE 02-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-02.lhs

-- EXERCISE 01========================================================================

-- Define a function that returns a list without the first three elements and
-- last three elements.
ex211 :: [a] -> [a]
ex211 = middleOfList
middleOfList xs = drop 3 $ take (length xs - 3) xs

-- Define a function 'initals s1 s2' that takes a person's name and a surname 
-- as input and returns a string consisting of person's initials.
-- initials "James" "Bond" => "J. B."
ex212 :: String -> String -> String
ex212 = initials
initials s1 s2 = head s1 : ". " ++ head s2 : "."

-- Define a function that concatenates two strings, so that the longest string
-- always comes first.
ex213 :: String -> String -> String
ex213 = concatLongerFirst
concatLongerFirst s1 s2 | length s1 >= length s2 = s1 ++ s2
                        | otherwise              = concatLongerFirst s2 s1

-- Define a function 'safeHead' that returns an empty list if 'l' is an empty
-- list, otherwise it returns its first element wrapped inside a singleton list.
ex214 :: [a] -> [a]
ex214 = safeHead
safeHead xs | null xs = []
            | otherwise = [head xs]

-- Define a function 'hasDuplicates' that checks whether a list contains
-- duplicate elements (use 'nub').
ex215 :: Eq a => [a] -> Bool
ex215 = hasDuplicates
hasDuplicates xs = (length xs) == (length $ nub xs)

-- EXERCISE 02========================================================================

-- Redefine 'doublesFromTo' so that it also works when b<a.
ex221 :: (Enum a, Num a, Ord a) => a -> a -> [a]
ex221 = doublesFromTo
doublesFromTo a b | a < b     = [x*2 | x <- [a..b]]
                  | otherwise = doublesFromTo b a

-- Redefine 'ceasarCode n xs' so that it shifts all letters a specified number 
-- of positions 'n', converts all input to lowercase, and ensures that letters 
-- remain within the ['a'..'z'] interval.
ex222 :: Int -> String -> String
ex222 = caesarCode
caesarRange = ['a'..'z']
caesarCode n xs = [toLower $ head $ (drop (ord c - ord 'a' + n) $ cycle caesarRange) | c <- xs, c /= ' ']
-- Explanation: Cycle the allowed caesar range and drop everything up until the current
-- characters symbol and then drop again n elements so we get the next shifted character


-- EXERCISE 03========================================================================

-- Define 'letterCount' that computes the total number of letters in a string,
-- thereby ignoring the whitespaces and all words shorter than three letters.
-- You can use 'totalLength'.
ex231 :: String -> Int
ex231 = letterCount
letterCount s = sum [length w | w <- words s, length w >= 3]

-- Redefine 'isPalindrome' so that it's case insensitive and works correctly 
-- for strings that contain whitespaces.
ex232 :: String -> Bool
ex232 = isPalindrome

-- Removes white spaces and puts chars to lower
removeWhiteSpaces :: String -> String
removeWhiteSpaces s = [toLower c | c <- s, c /= ' ']

isPalindrome s =  (removeWhiteSpaces s) == (reverse $ removeWhiteSpaces s)

-- Other variant with recursion
isPalindrome' s | length s < 2                             = True
                | head s == ' '                            = isPalindrome' $ tail s
                | last s == ' '                            = isPalindrome' $ init s
                | (toLower $ head s) == (toLower $ last s) = isPalindrome' $ init $ tail s
                | otherwise                                = False

-- Define 'flipp xss' that takes a list of lists, reverts each individual list,
-- and concatenates all of them, but in the reverse order.
-- flip ["water","is","warm"] -> "mrawsiretaw"
ex233 :: [[a]] -> [a]
ex233 = flipp
flipp xss = concat $ [reverse xs | xs <- reverse xss]

-- EXERCISE 04========================================================================

-- Define 'inCircle r x y' that returns the coordinates of all points within
-- the ([-10..10],[-10..10]) interval that fall inside a circle of radius
-- 'r' with center '(x,y)'.

-- Redefine the function so that it takes the resolution of the grid as an 
-- additional argument.
ex241 :: Double -> Double -> Double -> [(Double, Double)]
ex241 = inCircle
inCircleGrid :: Double -> Double -> Double -> [Double] -> [Double] -> [(Double, Double)]
inCircleGrid r x y gridX gridY = [(a, b) | a <- gridX, b <- gridY, (a - x)^2 + (b - y)^2 <= r^2]
inCircle r x y = inCircleGrid r x y [-10..10] [-10..10]

-- Define 'steps xs' that, given a list xs=[x1,x2,..], generates the pairs
-- [(x1,x2),(x2,x3),...]. Hint: have a look at 'pairs5'.
ex242 :: [a] -> [(a, a)]
ex242 = steps
steps xs = zip xs $ tail xs

-- EXERCISE 05========================================================================

-- Define 'indices x xs' that returns the indices of element 'x' in list 'xs'
-- (if 'x' appears multiple times, there will be a number of such indices).
-- indices 'a' "alphabet" => [0, 4]
ex251 :: Eq a => a -> [a] -> [Int]
ex251 = indices
indices x xs = [fst ix | ix <- zip [0..] xs, snd ix == x]

-- Define 'showLineNumbers s' that prefixes all lines from string 's' with a
-- line number.
-- showLineNumbers "first line\nsecond line" => "1 first line\n2 second line\n"
ex252 :: String -> String
ex252 = showLineNumbers
showLineNumbers s = unlines [(show $ fst ix) ++ " " ++ snd ix | ix <- zip [1..] $ lines s]

-- Define 'haveAlignment xs ys' that returns 'True' if 'xs' and 'ys' have
-- any identical elements that are aligned (appear at the same position in
-- both lists)
ex253 :: Eq a => [a] -> [a] -> Bool
ex253 = haveAlignment
haveAlignment xs ys = or [fst ix == snd ix | ix <- zip xs ys]

-- Define 'common xs ys' that returns the aligned subsequences.
-- haveAlignment "water" "fire" => True
-- common "witer" "fire" => "ie"
ex254 :: Eq a => [a] -> [a] -> [a]
ex254 = common
common xs ys = [fst ix | ix <- zip xs ys, fst ix == snd ix]

{-LECTURE 03-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-03.lhs

-- EXERCISE 01========================================================================

-- Without using the ':t' command, determine the types of the following 
-- functions:
foo10 :: String -> [String]
foo10 w = [x ++ y | x <- lines w, y <- lines w]

foo11 :: String -> [(String, String)]
foo11 w = [(x,y) | x <- lines w, y <- lines w]

foo12 :: String -> [String]
foo12 w = [y : x | x <- lines w, y <- w]

foo13 :: String -> [(String, String)]
foo13 w = [(y:x, w) | x <- lines w, y <- w]

foo14 :: String -> [(Char, Bool)]
foo14 w = [(x, x=='a') | x <- w ]

foo15 :: String -> String
foo15 s = tail [ c | c <- s, isLower c ]

foo16 :: String -> [(Char, Char)]
foo16 s = zip [ c | c <- s, isLower c ] "Haskell"

foo17 :: Int -> Char -> String
foo17 n c = reverse $ drop n $ c : "Haskell" 

foo18 :: String -> String
foo18 xs = last $ words xs

foo19 :: Char -> String -> String
foo19 x z = x : 'y' : z

-- EXERCISE 02========================================================================

-- Without using the ':t' command, determine the types of the following 
-- functions:

foo20 :: [a] -> [a]
foo20 xs = tail xs ++ [head xs]

foo21 :: [a] -> (a, [a])
foo21 xs = (head xs, tail xs)

foo22 :: a -> [a] -> [a]
foo22 x xs = x:xs

foo23 :: [a] -> [a]
foo23 l = init $ tail l

foo24 :: [[a]] -> [a] -> [a]
foo24 xss ys = concat xss ++ ys

foo25 :: [[a]] -> [a] -> (a, a)
foo25 xss ys = (head $ concat xss, head ys)

foo26 :: [[[a]]] -> a
foo26 xs = head $ concat $ concat xs

foo27 :: [a] -> [[a]]
foo27 cs = [[c1,c2] | c1 <- cs, c2 <- cs]

foo28 :: [[a]] -> [[a]]
foo28 cs = [concat [c1,c2] | c1 <- cs, c2 <- cs]

foo29 :: [a] -> [a]
foo29 cs = concat [[c1,c2] | c1 <- cs, c2 <- cs]

-- EXERCISE 03========================================================================

-- Without using the ':t' command, determine the types of the following 
-- functions:

foo30 :: Eq a => a -> [a] -> a
foo30 x ys = if x==head ys then x else last ys

foo31 :: Ord a => a -> [a] -> a
foo31 x ys = if x < head ys then x else last ys

foo32 :: Eq a => [a] -> [[a]] -> a
foo32 xs yss = if xs==head yss then head xs else last xs

foo33 :: Enum a => Bool -> [a] -> [(Int, a)]
foo33 x ys = if x then zip [1..9] ys else []

foo34 :: String -> [(Int, String)]
foo34 w = zip [0..] (lines w)

foo35 :: (Integral a, Fractional a) => a -> a -> a
foo35 x y = if odd x then y else x / 10

foo36 :: Ord a => [a] -> Bool
foo36 xs = sort xs == xs

foo37 :: Show a => [a] -> [[a]] -> String
foo37 x xs = show x ++ (show $ concat xs)

foo38 :: Num a => [[a]] -> a
foo38 xs = sum $ concat xs

foo39 :: (Num a, Ord a) => [a] -> [[a]] -> a
foo39 xs yss = sum $ [min x y | x <- xs, ys <- yss, y <- ys]
