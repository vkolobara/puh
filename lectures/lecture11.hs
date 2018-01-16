import Control.Exception
import Data.Char
import Data.List
import qualified Data.Map as M
import Control.Monad
import System.IO
import System.Directory
import System.IO.Error
import System.Environment
import System.FilePath
import System.Random
import System.FilePath

-- EXERCISE 1

main = do
  x <- getLine
  y <- getLine
  putStrLn . reverse $ x++y

---------------

-- EXERCISE 2

threeStrings :: IO Int
threeStrings = do
  s1 <- getLine
  s2 <- getLine
  s3 <- getLine
  let s  = s1++s2++s3
  putStrLn $ s
  return $ length s

askNumber9 :: IO Int
askNumber9 = do
  number <- getLine
  if checkDigits number then return $ read number else askNumber9

checkDigits = all isDigit
-----------
-- 

-- EXERCISE 3

f1 = do
  num <- getLine
  xs <- replicateM (read num) getLine
  mapM_ print (reverse xs)

sequence1 xs = sequence123 xs []
sequence123 []     acc = return acc
sequence123 (x:xs) acc = do
  val <- x
  sequence123 xs (val:acc)

--------------

-- EXERCISE 4

filterOdd :: IO ()
filterOdd = do
  s <- getContents
  putStrLn . unlines . map snd . filter (\(x,y) -> even x) $ zip [0..] (lines s)

numberLines :: IO ()
numberLines = do
  s <- getContents
  putStrLn . unlines . map (\(x,y) -> show x ++ ' ':y) $ zip [0..] (lines s)

---------------------

wc :: FilePath -> IO (Int, Int, Int)
wc f = do
  h <- openFile f ReadMode
  s <- hGetContents h
  let chrs = length s
  let wrds = length . words $ s
  let lns = length . lines $ s
  return (chrs, wrds, lns)
