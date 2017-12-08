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

data Robot
data Bearing 

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = undefined

bearing :: Robot -> Bearing
bearing = undefined

coordinates :: Robot -> (Integer, Integer)
coordinates = undefined

{- It is MANDATORY to implement 'simulate' function in terms of fold -}
simulate :: Robot -> String -> Robot
simulate = undefined

advance :: Bearing -> (Integer, Integer) -> (Integer, Integer)
advance = undefined

turnLeft :: Bearing -> Bearing
turnLeft = undefined

turnRight :: Bearing -> Bearing
turnRight = undefined

-- Task 02

data TriangleType

triangleType :: (Ord a, Num a) => a -> a -> a -> TriangleType
triangleType = undefined

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
splitter = undefined

-- Task 04

{-
  For this task either write a solution to the problem or if you think
  solution doesn't exist explain why that is the case. Of corse, solution
  must use fold.
-}