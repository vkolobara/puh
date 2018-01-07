{-# LANGUAGE NoMonomorphismRestriction #-}
--
module Exercises where
--
import Data.List
import Data.Char
import Data.Ord
import Data.Maybe
{-
    Here you should provide your solutions to in-class exercises.

    Make sure that ALL FUNCTIONS (including exXXX) have correct TYPE SIGNATURES.

    You should include solutions from following lectures :
    http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-08.lhs
    http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-09.lhs

    DON'T change function names, just remove 'undefined' and write your own
    definition for that function.
-}

{-LECTURE 08-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-08.lhs

-- EXERCISE 01 =======================================================================
{-
  Define the following functions using composition and pointfree style (you may
  of course use local definitions):
-}

{-
  1.1.
  - Define 'sumEven' that adds up elements occurring at even (incl. zero) 
    positions in a list.
    sumEven :: [Integer] -> Integer
    sumEven [1..10] => 25
-}

sumEven :: [Integer] -> Integer
sumEven = sum . map snd . filter (even . fst) . zip [0..]
{-
  1.2.
  - Define 'filterWords ws s' that removes from string 's' all words contained
    in the list 'ws'.
    filterWords :: [String] -> String -> String
-}

filterWords :: [String] -> String -> String
filterWords ws = unwords . filter (`notElem` ws) . words 

{-
  1.3.
  - Define 'initials3 d p s' that takes a string 's' and turns it into a string
    of initials. The function is similar to 'initials2' but additionally delimits
    the initials with string 'd' and discards the initials of words that don't
    satisfy the predicate 'p'.
    initials3 :: String -> (String -> Bool) -> String -> String
    initials3 "." (/="that") "a company that makes everything" => "A.C.M.E."
  - Use this function to define the 'initials' function.
-}

initials3 :: String -> (String -> Bool) -> String -> String
initials3 d p =  concat . map ((:'.':[]) . toUpper . head) . filter p . words 
initials :: String -> String
initials = initials3 "." (/="")

-- EXERCISE 02 =======================================================================
{-
  Just a reminder that EVERY function in this file needs to have a type signature ;)
-}

{-
  2.1.
  - Define 'maxDiff xs' that returns the maximum difference between consecutive
    elements in the list 'xs'.
    maxDiff :: [Int] -> Int
    maxDiff [1,2,3,5,1] => 4
  - Define 'maxMinDiff' that returns the pair (min_difference, max_difference).
-}

maxDiff :: [Int] -> Int
maxDiff xs = maximum . map (abs . uncurry (-)) . zip xs $ tail xs

maxMinDiff :: [Int] -> (Int,Int)
maxMinDiff xs = (minimum diff, maximum diff)
  where diff = map (abs . uncurry (-)) $ zip xs $ tail xs

{-
  2.2.
  - Define 'studentsPassed' that takes as input a list [(NameSurname,Score)] and
    returns the names of all students who scored at least 50% of the maximum 
    score.
-}
studentsPassed :: [(String,Double)] -> [String]
studentsPassed xs = map fst $ filter ((>=med) . snd) xs
  where med = (/2) . maximum $ map (snd) xs

-- EXERCISE 03 =======================================================================
{-
  3.1.
  - Define 'isTitleCased' that checks whether every word in a string is
    capitalized.
    isTitleCased :: String -> Bool
    isTitleCased "University Of Zagreb" => True
-}

isTitleCased :: String -> Bool
isTitleCased = all (isUpper . head) . words 

{-
  3.2.
  - Define 'sortPairs' that sorts the list of pairs in ascending order with
    respect to the second element of a pair.
-}

sortPairs :: Ord b => [(a,b)] -> [(a,b)]
sortPairs = sortBy (comparing snd)

{-
  3.3.
  - Define 'filename' that extracts the the name of the file from a file path.
    filename :: String -> String
    filename "/etc/init/cron.conf" => "cron.conf"
-}

filename :: String -> String
filename xs = drop ((maximum $ elemIndices '/' xs) + 1) xs 

{-
  3.4.
  - Define 'maxElemIndices' that returns the indices of the maximum element in a
    list. Return "empty list" error if the list is empty.
    maxElemIndices :: Ord a => [a] -> [Int]
    maxElemIndices [1,3,4,1,3,4] => [2,5]
-}

--maxElemIndices :: Ord a => [a] -> [Int]
maxElemIndices xs = fst $ foldr f ([],head xs) (zip [0..] xs)
  where f (a,b) (i,m)
          | b > m     = (a:[], b)
          | b == m    = (a:i, m)
          | otherwise = (i,m)

-- EXERCISE 04 =======================================================================

{-
  4.1. 
  - Define 'elem'' using 'foldr'.
-}

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' x = foldr (\y acc -> x == y || acc) False 

{-
  4.2.
  - Define 'reverse' using 'foldr'.
-}

reverse' :: [a] -> [a]
reverse' = foldr (\y acc -> acc ++ [y]) []
  
{-
  4.3.
  - Using 'foldr' define 'nubRuns' that removes consecutively repeated elements
    from a list.
    nubRuns :: Eq a => [a] -> [a]
    nubRuns "Mississippi" => "Misisipi"
-}

nubRuns :: Eq a => [a] -> [a]
nubRuns = foldr (\y acc -> if null acc || y /= head acc then y:acc else acc ) []

-- EXERCISE 05 =======================================================================

{-
  5.1.
  - Write 'reverse' using 'foldl'.
    reverse' :: [a] -> [a]
-}

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc ) [] 

{-
  5.2.
  - Using 'foldl' define function 'sumEven' from problem 1.1.
-}

sumEven' :: [Integer] -> Integer
sumEven' = snd . foldl (\(a,b) x -> if even a then (a+1,b+x) else (a+1,b)) (0,0)

{-
  5.3.
  - Using 'foldl' define maxUnzip :: [(Int,Int)] -> (Int,Int) 
    that returns the maximum element at first position in a pair and maximum
    element at second position in the pair. In other words, the function should
    be equivalent to:
      maxUnzip zs = (maximum xs, maximum ys)
        where (xs,ys) = unzip zs
    Return "empty list" error if the list is empty.
-}

maxUnzip :: [(Int,Int)] -> (Int,Int) 
maxUnzip []         = error "Cannot unzip empty list"
maxUnzip ((a,b):zs) = foldl (\(a,b) (c,d) -> (a `max` c, b `max` d)) (a,b) zs

{-LECTURE 09-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-09.lhs

-- EXERCISE 01 =======================================================================

{-
  1.1.
  - Define a 'Date' structure with the appropriate fields.
  - Define a function that shows a date in the DD.MM.YYYY format (without
    leading zeroes).
    showDate :: Date -> String
-}

data Date = Date Int Int Int deriving Show

showDate :: Date -> String
showDate (Date x y z) = show(x) ++ "." ++ show(y) ++ "." ++ show(z) ++ "."

{-
  1.2.
  - Define a function
    translate :: Point -> Shape2 -> Shape2
    that translates a shape into the direction of vector (x,y).
-}

data Point = Point Double Double 
  deriving Show

data Shape2 = Circle2 Point Double | Rectangle2 Point Point 
  deriving Show

translate :: Point -> Shape2 -> Shape2  
translate (Point x y) (Circle2 (Point p q) r) = Circle2 (Point (x + p) (y + q)) r
translate (Point x y) (Rectangle2 (Point x1 y1) (Point x2 y2)) = Rectangle2 (Point (x + x1) (y + y1)) (Point (x + x2) (y + y2))

{-
  1.3.
  - Write a function 'inShape' that tests whether a point is contained within a
    given shape (or is on its border).
    inShape :: Shape2 -> Point -> Bool
  - Write a function 'inShapes' that tests if the point is within any shape from
    the list of shapes.
    inShapes :: [Shape2] -> Point -> Bool
-}

inShape :: Shape2 -> Point -> Bool
inShape (Circle2 (Point p q) r) (Point x y)        = x <= p + r && x >= p - r && y <= q + r && y >= q - r
inShape (Rectangle2 (Point a b) (Point c d)) (Point x y) = x >= a && x <= c && y >= d && y <= b



inShapes :: [Shape2] -> Point -> Bool
inShapes xs p = and $ map (\x -> inShape x p) xs

{-
  1.4.
  - Define your type 'Vehicle' that can be a 'Car', 'Truck', 
    'Motorcycle', or 'Bicycle'. The first three store a name of the manufacturer
    (String) and horsepower (Double).
  - Write a function 'totalHorsepower' that adds up the horsepower of the
    vehicles, assuming that bicycle's horsepower is 0.2.
-}

data Vehicle =   Car String Double
               | Truck String Double
               | Motorcycle  String Double 
               | Bicycle deriving Show

totalHorsepower :: [Vehicle] -> Double
totalHorsepower = sum . map f 
  where f (Bicycle)         = 0.2
        f (Car _ hp)        = hp
        f (Truck _ hp)      = hp
        f (Motorcycle _ hp) = hp


-- EXERCISE 02 =======================================================================

data Level = Bachelor | Master | PhD deriving (Show, Eq)

data Student = Student
  { firstName  :: String
  , lastName   :: String
  , studentId  :: String
  , level      :: Level
  , avgGrade   :: Double
  } deriving Show

{-
  2.1.
  - Define a function that increases the average grade of the student by 1.0,
    but not above 5.0.
    improveStudent :: Student -> Student
-}

improveStudent :: Student -> Student
improveStudent stud = stud { avgGrade = x}
  where x = if (avgGrade stud >= 4.0) then 5.0 else avgGrade stud + 1.0

{-
  2.2.
  - Write a function to compute the average grade of students for the different
    study levels.
    avgGradePerLevels :: [Student] -> (Double, Double, Double)
-}

avgGradePerLevels :: [Student] -> (Double, Double, Double)
avgGradePerLevels xs = (a / al, b / bl, c / cl)
  where (a, b, c, al, bl, cl) = foldr f (0.0, 0.0, 0.0, 0, 0, 0) xs 
        f x (b, m, p, bl, ml, pl)
          | level x == Bachelor = (b + avgGrade x, m, p, bl + 1, ml, pl)
          | level x == Master   = (b, m + avgGrade x, p, bl, ml + 1, pl)
          | level x == PhD      = (b, m, p + avgGrade x, bl, ml, pl + 1)

{-
  2.3.
  - Write a function that returns a list of matriculation numbers for a given
    study level, sorted by average grade in descending order.
    rankedStudents :: Level -> [Student] -> [String]
-}

rankedStudents :: Level -> [Student] -> [String]
rankedStudents l xs = map (studentId) $ sortBy (comparing avgGrade) $ filter ((==l) . level) xs

{-
  2.4.
  - Write a function
    addStudent :: Student -> [Student] -> [Student]
    that adds a student to a list of students. If a student with an identical
    matriculation number already exists in the list, the function should return an
    error. 
-}

addStudent :: Student -> [Student] -> [Student]
addStudent s xs
  | studExist s xs = error "Student with identical matriculation number already exists."
  | otherwise      = s : xs
    where studExist s = or . map ((==(studentId s)) . studentId)

-- EXERCISE 03 =======================================================================

{-
  3.1.
  - Define your own parametrized type 'MyTriplet' that contains the values of
    three different types. Do this using a record.
  - Define a function 
    toTriplet :: MyTriplet a b c -> (a, b, c)
    that converts a 'MyTriplet' value into an ordinary triplet.
-}

data MyTriplet a b c = MyTriplet
  { x :: a,
    y :: b,
    z :: c
} deriving Show

toTriplet :: MyTriplet a b c -> (a, b, c)
toTriplet (MyTriplet a b c)= (a, b, c)

{-
  3.2.
  - Define a function (Employee - salary :: Maybe Double, name :: String) deriving Show
    totalSalaries :: [Employee] -> Double
    that sums the known salaries of employees (salaries that are not 'Nothing').
-}

data Employee = Employee
  { name   :: String
  , salary :: Maybe Double
  } deriving Show

totalSalaries :: [Employee] -> Double
totalSalaries xs = sum $ map (fromMaybe 0 . salary) xs

{-
  3.3.
  - Write a function 'addStudent2' that works like 'addStudent' from problem 2.4
    but returns a 'Maybe' type instead of an error.
    addStudent2 :: Student -> [Student] -> Maybe [Student]
  - Write 'addStudent3' that returns an 'Either'.
-}

addStudent2 :: Student -> [Student] -> Maybe [Student]
addStudent2 s xs
  | studExist s xs = Nothing
  | otherwise      = Just $ s : xs
    where studExist s = or . map ((==(studentId s)) . studentId)

addStudent3 :: Student -> [Student] -> Either String [Student]
addStudent3 s xs
  | studExist s xs = Left "Student with identical matriculation number already exists."
  | otherwise      = Right $ s : xs
    where studExist s = or . map ((==(studentId s)) . studentId)