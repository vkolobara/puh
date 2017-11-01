module Homework where
--
import Data.List
import Data.Char
import Data.Bits ( xor )
--

-- Task 01
localMaxima :: [Int] -> [Int]
localMaxima []         = []
localMaxima (p:x:s:xs)
  | x > p && x > s     = x : localMaxima (x:s:xs)
  | otherwise          = localMaxima (x:s:xs)
localMaxima (x:xs)     = localMaxima xs

-- Task 02
transform :: [(Int, String)] -> [(Char, Int)]
transform xs = concat [extractChars val chars | (val, chars) <- xs]
  where
    extractChars val chars = [(toLower c, val) | c <- chars]

-- Task 03
rule90 :: [Bool] -> [[Bool]]
rule90 xs = xs : rule90' xs
  where 
    rule90' ys  = step : rule90' step
      where step = rule90Step ys

rule90Step :: [Bool] -> [Bool]
rule90Step []     = []
rule90Step (x:y:xs) = y : rule90Step' (x:y:xs)
  where
    rule90Step' []         = []
    rule90Step' (p:x:s:xs) = p `xor` s : rule90Step' (x:s:xs)
    rule90Step' (x:s:xs)   = x         : rule90Step' (s:xs)
    rule90Step' _          = [False] 
rule90Step (x:xs) = [False]

pretty :: [[Bool]] -> String
pretty []       = ""
pretty (bs:bss) = lineToString bs ++ "\n" ++ pretty bss
  where boolToChar     False = ' '
        boolToChar     True  = '#'
        lineToString bools   = [boolToChar b | b <- bools]

-- Task 04
f :: [String]
f = "1" : f' "1"

f' s = next : f' next
  where next = sequenceString s

sequenceString "" = ""
sequenceString s  = showSequence seq ++ sequenceString rem
  where (seq, rem) = splitSequence s

splitSequence :: String -> (String, String)
splitSequence []     = ("", "")
splitSequence (x:xs) = splitSequence' x (x:xs)
  where splitSequence' c s = (takeWhile (== c) s, dropWhile (== c) s)

showSequence :: String -> String
showSequence "" = ""
showSequence s  = (show $ length s) ++ [head s]


