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

data Robot = Robot Bearing (Integer,Integer) deriving Show
data Bearing = N | S | E | W deriving Show

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot b (x,y) = Robot b (x,y)

bearing :: Robot -> Bearing
bearing (Robot b (x,y)) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot b (x,y)) = (x, y)

move :: Robot -> Char -> Robot
move (Robot b (x, y)) 'A' = Robot b (advance b (x, y))
move (Robot b (x, y)) 'L' = (Robot (turnLeft b) (x, y))
move (Robot b (x, y)) 'R' = (Robot (turnRight b) (x, y))

{- It is MANDATORY to implement 'simulate' function in terms of fold -}
simulate :: Robot -> String -> Robot
--simulate (Robot b (x,y)) = foldl (\acc n -> move n acc) (Robot b (x,y))
simulate = foldl move

advance :: Bearing -> (Integer, Integer) -> (Integer, Integer)
advance N (x, y) = (x, y + 1)
advance E (x, y) = (x + 1, y)
advance S (x, y) = (x, y - 1)
advance W (x, y) = (x - 1, y)

turnLeft :: Bearing -> Bearing
turnLeft N = W
turnLeft W = S
turnLeft S = E
turnLeft E = N

turnRight :: Bearing -> Bearing
turnRight N = E
turnRight E = S
turnRight S = W
turnRight W = N

-- Task 02

data TriangleType = Equilateral
                  | Degenerate
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving Show

triangleType :: (Ord a, Num a) => a -> a -> a -> TriangleType
triangleType x y z = if x == 0 || y == 0 || z == 0 then Illegal else
                     if x + y < z || x + z < y || y + z < x then Illegal else
                     if x + y == z || x + z == y || y + z == x then Degenerate else
                     if x == y && y == z then Equilateral else
                     if x == y || x == z || y == z then Isosceles else
                     if x /= y && x /= z && y /= z then Scalene else
                     Illegal

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
splitter delim = let
  f (x, y) = x ++ [y]
  foldFn (l, curr) c = let
                       str = curr ++ [c]
                       n = (length str) - (length delim)
                       in if isSuffixOf delim str
                             then (l ++ [take n str], [])
                             else (l, str)

  in f . foldl foldFn ([], [])

-- Task 04

{-
  For this task either write a solution to the problem or if you think
  solution doesn't exist explain why that is the case. Of corse, solution
  must use fold.
-}


{-
No, we can't solve this problem on infinite lists using foldr, foldl or foldl'.

There are two choices for folds on the infinite list, foldr and foldl.
foldl f acc (x:xs) = foldl f (f x acc) xs
foldr f acc (x:xs) = f x (foldr f acc xs)

foldl is automatically out of the question since it doesn't let f take control until end of the list is reached - which is never, in the case of an infinite list.
foldl' also doesn't help - it just makes the whole thing strict, but we still have to wait for the infinite list to end.

So the only thing we can use is foldr. The only way we can partially evaluate foldr on an infinite list is when the function f ignores the second argument, which in our case are the other letters in the string.
But we can't ignore the other letters, as the only way we can make a decision to split is to look at all the upcoming letters.

Below is an implementation of splitter using right fold; it also breaks down on infinite lists.
-}

splitterR :: Eq a => [a] -> [a] -> [[a]]
splitterR delim = let
  f (x, y) = y:x
  foldFn c (l, curr)= let
                       str = c:curr
                       n = length delim
                       in if isPrefixOf delim str
                             then ((drop n str):l, [])
                             else (l, str)
  in f . foldr foldFn ([], [])

