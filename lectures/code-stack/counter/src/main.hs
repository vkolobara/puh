import Strings.StringOp (tokenize)
import Data.List (sortBy)
import Data.Ord (comparing)
import Counts (count)
import System.Environment
import System.IO

main = do
  (f:_) <- getArgs
  s <- readFile f
  putStr . unlines . map (\(s,c) -> show c ++ " " ++ s) . sortBy (flip $ comparing snd) . count $ tokenize s
