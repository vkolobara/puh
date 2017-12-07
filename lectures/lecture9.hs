import Data.List

data Point = Point Double Double 
  deriving Show
data Shape2 = Circle2 Point Double | Rectangle2 Point Point 
  deriving Show

-- Exercise 1

data Date = Date Int Int Int

showDate :: Date -> String
showDate (Date d m y) = show d ++ "." ++ show m ++ "." ++ show y 

translate :: Point -> Shape2 -> Shape2

translate (Point x y) (Circle2 (Point cx cy) r) = 
  Circle2 (Point (cx+x) (cy+y)) r

translate (Point x y) (Rectangle2 (Point x1 y1) (Point x2 y2)) = 
  Rectangle2 (Point (x+x1) (y+y1)) (Point (x+x2) (y+y2))

-----------------------------------------------------------------

-- RECORDS

data Level = Bachelor | Master | PhD deriving (Show, Eq)

data Student = Student
 { firstName  :: String
 , lastName   :: String
 , studentId  :: String
 , level      :: Level
 , avgGrade   :: Double } deriving Show

-- Exercise 2

improveStudent :: Student -> Student
improveStudent student = student { avgGrade = newGrade }
  where newGrade = max 5.0 (1 + avgGrade student)

avgGradePerLevels :: [Student] -> (Double, Double, Double)
avgGradePerLevels xs = avgGradePerLevels' xs (0,0,0) (0,0,0)

-- Fold or filter for more readable code (for each level)
-- More smaller functions is better (avgForLevel)
avgGradePerLevels' :: [Student] -> (Double, Double, Double) -> (Double,Double,Double) -> (Double,Double,Double)
avgGradePerLevels' [] (l1,l2,l3) (n1,n2,n3) = (l1/n1, l2/n2, l3/n3)
avgGradePerLevels' (x:xs) (lvl1, lvl2, lvl3) (cnt1, cnt2, cnt3)
  | lvl == Bachelor = avgGradePerLevels' xs (lvl1+avg, lvl2, lvl3) (cnt1+1,cnt2,cnt3)
  | lvl == Master = avgGradePerLevels' xs (lvl1, lvl2+avg, lvl3) (cnt1,cnt2+1,cnt3)
  | lvl == PhD = avgGradePerLevels' xs (lvl1, lvl2, lvl3+avg) (cnt1,cnt2,cnt3+1)
  | otherwise  = error "Bla"
  where lvl = level x
        avg = avgGrade x

-------------------------------------------------------------------

-- PARAMETRIZED TYPES

-- MAYBE
