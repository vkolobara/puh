import StringOp (tokenize)
import Counts (count)
import System.Environment
import System.IO

main = do
  (f:_) <- getArgs
  s <- readFile f
  print $ count $ tokenize s
