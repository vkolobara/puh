module Homework where
--
import           Data.List
--

-- Task 01

{-
  Inspect type signatures of functions that you are supposed to implement
  and and from that information try to think of how Robot and Bearing
  data types should look like.
-}

data Robot   = Robot { brng  :: Bearing
                     , coord :: (Integer, Integer) } deriving Show
data Bearing = NORTH | EAST | SOUTH | WEST deriving Show

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ c) = c

{- It is MANDATORY to implement 'simulate' function in terms of fold -}
simulate :: Robot -> String -> Robot
simulate = foldl doInstruction

doInstruction :: Robot -> Char -> Robot
doInstruction r@(Robot b _) 'R' = r {brng=turnRight b}
doInstruction r@(Robot b _) 'L' = r {brng=turnLeft b}
doInstruction r@(Robot b c) 'A' = r {coord=advance b c}
doInstruction _ _               = error "unknown instruction"

advance :: Bearing -> (Integer, Integer) -> (Integer, Integer)
advance NORTH (x,y) = (x,   y+1)
advance WEST  (x,y) = (x-1, y)
advance SOUTH (x,y) = (x,   y-1)
advance EAST  (x,y) = (x+1,   y)

turnLeft :: Bearing -> Bearing
turnLeft NORTH = WEST
turnLeft WEST  = SOUTH
turnLeft SOUTH = EAST
turnLeft EAST  = NORTH

turnRight :: Bearing -> Bearing
turnRight NORTH = EAST
turnRight EAST  = SOUTH
turnRight SOUTH = WEST
turnRight WEST  = NORTH

-- Task 02

data TriangleType = Equilateral | Isosceles | Scalene | Degenerate | Illegal deriving (Eq, Show)
data Triangle     = Triangle Int Int Int deriving Show

triangleType :: (Ord a, Num a) => a -> a -> a -> TriangleType
triangleType a b c
  | any (<=0) [a,b,c]           = Illegal
  | (not . all isValid) pairs   = Illegal
  | any isDegenerate pairs      = Degenerate
  | a == b && b == c            = Equilateral
  | numSame <= 2                = Isosceles
  | numSame == 3                = Scalene
  | otherwise                   = Illegal
  where numSame                 = (length . nub) [a,b,c]
        pairs                   = [((a, b), c), ((a, c), b), ((b, c), a)]
        isDegenerate ((x,y), z) = x+y == z
        isValid ((x, y), z)     = x + y >= z

-- Task 03

{- some convenient test examples -}
-- splitter " > " " > " => ["", ""]
-- splitter " > " "123 > " => ["123", ""]
-- splitter " > " "123 > 456 > 789" => ["123", "456", "789"]

{-
  you don't have to bother with splitting on an empty list e.g.:
  splitter "" "abcde" => ["a", "b", "c", "d", "e"]
-}

{- It is MANDATORY to implement 'splitter' function in terms of fold -}
splitter :: Eq a => [a] -> [a] -> [[a]]
splitter sublst = foldr f []
  where len = length sublst
        -- if we found the sublist on which we should split, then remember the current solution
        -- and prepend the empty list to it so that we start building another part.
        -- otherwise prepend everything to the head of the current parts.
        f x acc = if sublst `isPrefixOf` (x:curr) then []:drop len (x:curr):rest else (x:curr):rest
          where (curr, rest) = if null acc then ([], []) else (head acc, tail acc)

-- Task 04

{-
  For this task either write a solution to the problem or if you think
  solution doesn't exist explain why that is the case. Of corse, solution
  must use fold.
-}

-- My implementation of splitter has no problems with infinite lists:
infList :: String
infList = cycle "123 > 456 > "

takeFromInf :: Int -> [String]
takeFromInf n = (take n . splitter " > ") infList

first10000 :: [String]
first10000 = takeFromInf 10000
