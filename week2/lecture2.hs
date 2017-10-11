import Data.Char
import Data.List

l1 = [1, 2, 3]

l1' = 1:2:3:[]

l2 = head l1
l3 = tail l1

l4 = last l1
l5 = init l1

l6 = take 2 l1
l7 = drop 2 l1

l8 = reverse l1

l9 = repeat 3
l10 = cycle [1, 2, 3]
l11 = replicate 10 'a'

replicate' n x = take n $ repeat x

l12 = [1..100]

l13 = [1, 3..999]

l14 = [1..]

l15 = take 10 l14
l16 = head l14

l17 = tail l14
n = length l14

trim l = init $ tail l

blanks = repeat ' '

padTo10 s = s ++ take (10 - length s) blanks

l18 = head []

l19 = [[1,2], [3, 4]]

l20 = concat l19

m1 = minimum l13

e1 = l13 !! 17
e2 = l19 !! 1 !! 0

intToChar i = ['A'..] !! (i - 65)

l24 = nub [1, 2, 1, 1, 3, 3, 1, 1]

f1 = elem 1 [1, 2 ,3]
f2 = notElem 5 [1, 2, 3]

f3 = null []
f4 = null [1, 2, 3]



-- Exercises --------------------------------------------
func1 l = drop 3 $ take ((length l) -3) l

initals s1 s2 = head s1 : '.' : head s2 : '.' : []

concatLonger s1 s2 | length s1 >= length s2 = s1 ++ s2
                   | otherwise = s2 ++ s1

safeHead l | null l = []
           | otherwise = head l

hasDuplicates l = length l /= length (nub l)
---------------------------------------------------------



-- List comprehension
doubles = [x*2 | x <- [1..10]]

sums1 = [x + y | x <- [1..10], y <- [1..10]]
sums2 = [x + y | x <- [1..10], y <- [1..10], x<y]
sums3 = [x + y | x <- [1..10], y <- [1..10], x<y, odd x || even y]

lengths xss = [length xs | xs <- xss]


-- Exercises
doublesFromTo a b | b < a = doublesFromTo b a
                  | otherwise = [x*2 | x <- [a..b]]





-------------------------------------------------------


ws = words "Moja kobila Suzy cesto sece po pruzi"

ls = lines "Haiku je tezak\nSedamnaest slogova\nnije bas dovolj"

stream = unlines ls

capitalized s = [w | w <- words s, isUpper $ head w]

camelCase s = concat [toUpper (head w) : tail w | w <- words s]

camelCase' s = concat [toUpper h : t | (h:t) <- words s]

-- Exercise

letterCount s = length $ concat [word | word <- words s, length word > 3]


--------------------------------------------------------------------------


-- Dovrsi tuples kod kuce
