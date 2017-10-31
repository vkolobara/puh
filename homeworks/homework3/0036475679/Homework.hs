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
rule90 = undefined

rule90Step :: [Bool] -> [Bool]
rule90Step = undefined

pretty :: [[Bool]] -> String
pretty = undefined

-- Task 04
f :: [String]
f = undefined
