module Main where

import Expression
import Parser
import Programs
import Statement
import System.IO
import Data.Maybe

main :: IO ()
main = do
  s   <- readFile "test.txt"
  let ast = Parser.parse s
  putStrLn (show ast)
  putStrLn (show $ run (extend empty "In" 300) (fromMaybe Skip ast) "Out")
