module Counts where

import Data.List (group, sort)

count :: Ord a => [a] -> [(a, Int)]
count xs = [ (y, length ys) |  ys@(y:_) <- group $ sort xs]
