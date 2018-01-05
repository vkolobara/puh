module Exec where

import Statement
import Expression
import System.IO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  s    <- readFile $ head args
  let ast = read s :: Statement
  putStrLn . show $ run empty ast "Out"
