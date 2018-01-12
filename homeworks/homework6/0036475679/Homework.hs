module Homework where
--
import           Data.List
import           Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength)
import           Data.Time.Clock    (UTCTime (..))
import           Data.Time.Format   (defaultTimeLocale, formatTime)
import           Text.Read
--

-- Task 01
data Pred = And Pred Pred
          | Or  Pred Pred
          | Not Pred
          | Val Bool deriving Show

eval :: Pred -> Bool
eval (Val b)     = b
eval (Not p)     = not $ eval p
eval (And p1 p2) = eval p1 && eval p2
eval (Or  p1 p2) = eval p1 || eval p2

-- Task 02

data Month = January
           | February
           | March
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December deriving (Show, Read, Eq, Enum)

data DayDesc = Monteenth
             | Tuesteenth
             | Wednesteenth
             | Thursteenth
             | Friteenth
             | Saturteenth
             | Sunteenth deriving (Show, Read, Eq, Enum)

data DayOfWeek = Monday
               | Tuesday
               | Wednesday
               | Thursday
               | Friday
               | Saturday
               | Sunday deriving (Show, Read, Eq, Enum)

getFirstDayOffset :: Int -> Integer -> Int -> Int
getFirstDayOffset dayOfWeek year month = if diff < 0 then 8 + diff else diff + 1
  where dayInt  = getDayOfWeek $ formatTime defaultTimeLocale "%A" fstDate
        fstDate = fromGregorian year month 1
        diff    = dayOfWeek - dayInt

getRightDay :: String -> Int -> Integer -> Int -> Day
getRightDay "first"  dayOfWeek year month = fromGregorian year month (getFirstDayOffset dayOfWeek year month)
getRightDay "second" dayOfWeek year month = fromGregorian year month (7 + getFirstDayOffset dayOfWeek year month)
getRightDay "third"  dayOfWeek year month = fromGregorian year month (14 + getFirstDayOffset dayOfWeek year month)
getRightDay "fourth" dayOfWeek year month = fromGregorian year month (21 + getFirstDayOffset dayOfWeek year month)
getRightDay "fifth"  dayOfWeek year month = if offset + 28 > gregorianMonthLength year month then error "no fifth day"
                                            else fromGregorian year month (28 + offset)
  where offset = getFirstDayOffset dayOfWeek year month

getRightDay "last"   dayOfWeek year month = day
  where day = case ((gregorianMonthLength year month - dayOfWeek) `div` 7)+1 of
                    4 -> getRightDay "fourth" dayOfWeek year month
                    _ -> getRightDay "fifth" dayOfWeek year month

getRightDayDsc :: Int -> Integer -> Int -> Day
getRightDayDsc dayDesc year month = fromGregorian year month (offset + (7 * ((19 - offset) `div` 7)))
  where offset = getFirstDayOffset dayDesc year month

example :: String
example = "the first Monday of March 2017"

getMonth :: String -> Int
getMonth s = enumToInt (readMaybe s :: Maybe Month)

getDayOfWeek :: String -> Int
getDayOfWeek s = enumToInt (readMaybe s :: Maybe DayOfWeek)

getDayDesc :: String -> Int
getDayDesc s = enumToInt (readMaybe s :: Maybe DayDesc)

enumToInt :: (Enum a) => Maybe a -> Int
enumToInt (Just m) = fromEnum m + 1
enumToInt _        = error "unknown value"

dateFromDescription :: String -> Day
dateFromDescription = parseDescriptionList . words


parseDescriptionList :: [String] -> Day
parseDescriptionList [_, dsc, dayStr, _, monthStr, yearStr] = day
  where year = read yearStr
        month = getMonth monthStr
        day = getRightDay dsc (getDayOfWeek dayStr) year month
parseDescriptionList [_, daydsc, _, monthStr, yearStr]   = day
  where year = read yearStr
        month = getMonth monthStr
        day = getRightDayDsc (getDayDesc daydsc) year month
parseDescriptionList _ = error "wrong input"

-- Task 03

data Tree a
  = Leaf | Node a (Tree a) (Tree a)
  deriving (Eq, Show)

-- a)
treeFilter :: (a -> Bool) -> Tree a -> Tree a
treeFilter p (Node val left right) = if p val then Node val (treeFilter p left) (treeFilter p right) else Leaf
treeFilter _ Leaf                  = Leaf

-- b)
levelMap :: (Int -> a -> b) -> Tree a -> Tree b
levelMap f tree = levelMap' tree 0
  where levelMap' (Node val left right) depth = Node (f depth val) (levelMap' left (depth+1)) (levelMap' right (depth+1))
        levelMap' Leaf _                      = Leaf
-- c)
isSubtree :: Eq a => Tree a -> Tree a -> Bool
isSubtree = undefined

-- Task 04
data Category

parseCategories :: [String] -> [Category]
parseCategories = undefined

printCategories :: [Category] -> [String]
printCategories = undefined
