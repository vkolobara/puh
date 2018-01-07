module Homework where
--
import Data.List
--

-- Task 01

{-
  Inspect type signatures of functions that you are supposed to implement
  and and from that information try to think of how Robot and Bearing 
  data types should look like.
-}

data Robot = Robot Bearing (Integer, Integer) deriving Show
data Bearing = North | South | West | East deriving (Show, Eq)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot b p = Robot b p

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ p) = p

{- It is MANDATORY to implement 'simulate' function in terms of fold -}
simulate :: Robot -> String -> Robot
simulate (Robot b (x,y)) s = foldl (\(Robot b (x,y)) c -> decideAndExecute b (x,y) c) (Robot b (x,y)) s

decideAndExecute :: Bearing -> (Integer,Integer) -> Char -> Robot
decideAndExecute b (x,y) 'A' = Robot b $ advance b (x,y)
decideAndExecute b (x,y) 'L' = Robot (turnLeft b) (x,y)
decideAndExecute b (x,y) 'R' = Robot (turnRight b) (x,y)

advance :: Bearing -> (Integer, Integer) -> (Integer, Integer)
advance b (x,y) 
  | b == North = (x, y + 1)
  | b == South = (x, y - 1)
  | b == West  = (x - 1, y)
  | b == East  = (x + 1, y)

turnLeft :: Bearing -> Bearing
turnLeft b
  | b == North = West
  | b == South = East 
  | b == West  = South
  | b == East  = North

turnRight :: Bearing -> Bearing
turnRight b
  | b == North = East
  | b == South = West 
  | b == West  = North
  | b == East  = South

-- Task 02

data Triangle a = Triangle {
    tType :: TriangleType,
    x     :: a,
    y     :: a,
    z     :: a
} deriving Show

data TriangleType = Equilateral | Isosceles | Scalene | Degenerate | Illegal deriving Show

triangleType :: (Ord a, Num a) => a -> a -> a -> TriangleType
triangleType a b c
  | checkIllegal     a b c = Illegal
  | checkEquilateral a b c = Equilateral
  | checkIsosceles   a b c = Isosceles
  | checkDegenerate  a b c = Degenerate
  | otherwise              = Scalene
    where checkIllegal a b c     = a > bc || b > ac || c > ab
          checkEquilateral a b c = a == b && b == c
          checkIsosceles a b c   = a == b || b == c || a == c
          checkDegenerate a b c  = a == bc || b == ac || c == ab
          ab = a + b
          ac = a + c
          bc = b + c

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
splitter xs ys =  transform $ foldr f ([],[]) ys
  where transform (a,b) = b:a
        f c (l1,l2)
          | isPrefixOf xs (c:l2) = ((drop (length xs) (c:l2)) : l1, [])
          | otherwise            = (l1, c:l2)

-- Task 04

{-
  For this task either write a solution to the problem or if you think
  solution doesn't exist explain why that is the case. Of corse, solution
  must use fold.
-}
