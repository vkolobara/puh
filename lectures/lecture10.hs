import Data.List
import Control.Monad

-- RECURSIVE DATA STRUCTURES
--
data Sex = Male | Female deriving (Show,Read,Eq,Ord)

data Person2 = Person2 {
  personId2 :: String,
  forename2 :: String,
  surname2  :: String,
  sex2      :: Sex, 
  mother2   :: Maybe Person2,
  father2   :: Maybe Person2,
  partner2  :: Maybe Person2,
  children2 :: [Person2] } deriving (Show,Read,Eq,Ord)

john = Person2 "123" "John" "Doe" Male Nothing Nothing (Just jane) []
jane = Person2 "623" "Jane" "Fox" Female (Just ann) Nothing (Just john) []
ann  = Person2 "343" "Ann"  "Doe" Female Nothing Nothing Nothing [jane]

-- Exercise 1

parentCheck :: Person2 -> Bool
parentCheck pers = (isChild pers mother) && (isChild pers father)
  where mother = mother2 pers
        father = father2 pers

isChild :: Person2 -> Maybe Person2 -> Bool
isChild pers1 (Just pers2) = (personId2 pers1) `elem` (map personId2 . children2) pers2
isChild pers1 Nothing = False


sister :: Person2 -> Maybe Person2
sister pers = if null children then Nothing else Just (head children)
  where mother    = mother2 pers
        father    = father2 pers
        mChildren = findFemaleChildren mother
        fChildren = findFemaleChildren father
        children  = mChildren ++ fChildren

findFemaleChildren :: Maybe Person2 -> [Person2]
findFemaleChildren Nothing     = []
findFemaleChildren (Just pers) = filter (\p -> sex2 p == Female) . children2 $ pers 

descendant :: Person2 -> [Person2]
descendant = descendant' []

descendant' :: [String] -> Person2 -> [Person2] 
descendant' visited p = if (elem id visited || null ch) then []
                        else ch ++ (concatMap (descendant' nv) $ ch)
  where id = personId2 p 
        ch = children2 p
        nv = id:visited

--------------------------------------------

-- Exercise 2

data MyList a = Empty | Cons a (MyList a) deriving (Show,Read,Ord)


listHead :: MyList a -> Maybe a
listHead Empty      = Nothing
listHead (Cons h _) = Just h

listMap :: (a -> b) -> MyList a -> MyList b
listMap f Empty        = Empty
listMap f (Cons a lst) = Cons (f a) (listMap f lst)

-------------------------------------------


-- Exercise 3

data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show)

treeMax :: Ord a => Tree a -> a
treeMax Null = error "empty tree"
treeMax (Node x Null Null) = x
treeMax (Node x left Null) = x `max` treeMax left
treeMax (Node x Null right) = x `max` treeMax right
treeMax (Node x left right) = max (treeMax left) (treeMax right)

treeToList :: Ord a => Tree a -> [a]
treeToList Null = []
treeToList (Node x left right) = treeToList left ++ [x] ++ treeToList right


levelCut :: Int -> Tree a -> Tree a
levelCut _ Null                = Null
levelCut 0 (Node x left right) = Node x Null Null
levelCut i (Node x left right) = Node x (levelCut (i-1) left) (levelCut (i-1) right)


treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Null = Node x Null Null
treeInsert x t@(Node y l r) 
  | x < y     = Node y (treeInsert x l) r
  | x > y     = Node y l (treeInsert x r)
  | otherwise = t

-- Exercise4 

listToTree :: Ord a => [a] -> Tree a
listToTree = foldl (flip treeInsert) Null 

sortAndNub :: Ord a => [a] -> [a]
sortAndNub = treeToList . listToTree

--------------------------------------------

--Deriving type class instances


--Defining type class instances


-- Exercise 5
data Weekday = 
  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show,Enum)

instance Eq Weekday where
  Monday    == Monday    = True
  Tuesday   == Tuesday   = True
  Wednesday == Wednesday = True
  Thursday  == Thursday  = True
  Saturday  == Saturday  = True
  Sunday    == Sunday    = True
  _         == _         = False

data Person = Person {
  idNumber :: String,
  forename :: String,
  surname  :: String,
  sex      :: Sex,
  age      :: Int,
  partner  :: Maybe Person,
  children :: [Person] } deriving (Eq,Read,Ord)

instance Show Person where
  show (Person id fn sn sx ag prt chldr) = 
    id ++ "," ++ 
    fn ++ "," ++
    sn ++ "," ++
    show sx ++ "," ++
    show ag ++ "," ++
    show (fmap forename prt) ++ "," ++
    show (map forename chldr)



-- Exercise 6
--

instance Eq a => Eq (MyList a) where
  Cons x _ == Cons y _ = x == y
  _        == _        = False

instance (Eq a,Ord a) => Eq (Tree a) where
  t1 == t2 = (nub . sort . treeToList) t1 == (nub . sort . treeToList) t2
