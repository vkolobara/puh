module Compile where

import Expression
import Parser
import Programs
import Statement
import System.IO
import System.Environment
import Data.Maybe

main :: IO ()
main = do
  args <- getArgs
  s   <- readFile $ args !! 0
  let ast = Parser.parse s
  if ast == Nothing then putStrLn "Parse errors!"
  else writeFile (args !! 1) (show ast)
