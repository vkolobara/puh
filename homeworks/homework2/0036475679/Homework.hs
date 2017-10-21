module Homework where

--
import Data.List
import Data.Char

--
-- Task 01
toRNA :: String -> String
nucleotidMapping :: Char -> Char
nucleotidMapping 'G' = 'C'
nucleotidMapping 'C' = 'G'
nucleotidMapping 'T' = 'A'
nucleotidMapping 'A' = 'U'
nucleotidMapping _   = error "No mapping exists"

toRNA s = [nucleotidMapping c | c <- s]

-- Task 02
transform :: [(Int, String)] -> [(Char, Int)]
transform xs = concat [extractChars val chars | (val, chars) <- xs]
  where
    extractChars val chars = [(c, val) | c <- chars]

-- Task 03
numberToWords :: Int -> String
numberToWords 1 = "one"
numberToWords 2 = "two"
numberToWords 3 = "three"
numberToWords 4 = "four"
numberToWords 5 = "five"
numberToWords 6 = "six"
numberToWords 7 = "seven"
numberToWords 8 = "eight"
numberToWords 9 = "nine"
numberToWords 10 = "ten"
numberToWords 11 = "eleven"
numberToWords 12 = "twelwe"
numberToWords 13 = "thirteen"
numberToWords 14 = "fourteen"
numberToWords 15 = "fifteen"
numberToWords 16 = "sixteen"
numberToWords 17 = "seventeen"
numberToWords 18 = "eighteen"
numberToWords 19 = "nineteen"
numberToWords 20 = "twenty"
numberToWords 30 = "thirty"
numberToWords 40 = "fourty"
numberToWords 50 = "fifty"
numberToWords 60 = "sixty"
numberToWords 70 = "seventy"
numberToWords 80 = "eighty"
numberToWords 90 = "ninety"
numberToWords n
  | n `div` 1000000 /= 0 = prefix 1000000 ++ " million" ++ suffix 1000000
  | n `div` 1000 /= 0    = prefix 1000 ++ " thousand" ++ suffix 1000
  | n `div` 100  /= 0    = prefix 100 ++ " hundred" ++ suffix 100
  | n `div` 10   /= 0    = numberToWords (n `div` 10 * 10) ++ "-" ++ numberToWords (n `mod` 10)
  | otherwise            = ""
  where
    suffix divisor = (if n `mod` divisor == 0 then "" else " ") ++
                     numberToWords (n `mod` divisor)
    prefix divisor = numberToWords $ n `div` divisor

-- Task 04
undefined' :: a
undefined' = error "undefined"
