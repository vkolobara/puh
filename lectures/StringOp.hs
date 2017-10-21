module StringOp 
  ( tokenize
  ) where

import Data.Char (toLower, isPunctuation)

lowerIt :: String -> String
lowerIt s = [toLower c | c <- s]

tokenize :: String -> [String]
tokenize s = [lowerIt w | w <- words $ removePunct s]

removePunct :: String -> String
removePunct s = [ c | c <- s, not $ isPunctuation c]


