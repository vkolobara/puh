import Data.Char
import Data.List

magicNumber :: Int -> String
magicNumber 42 = "Bla"
magicNumber x  = "Not Bla"

magicNumber2 :: Int -> String
magicNumber2 42 = "Bla"
magicNumber2 _ =  "Not Bla"

fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, y) = y

addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

swap (x, y) = (y, x)

leaves ((x, y), (z, w)) = [x, y, z, w]

-- PATTERN MATCHING


headHunter :: [[a]] -> a
headHunter ((x:_):_) = x
headHunter (_:(x:_):_) = x
headHunter (_:_:(x:_):_) = x
headHunter _           = error "Bla"

firstColumn :: [[a]] -> [a]
firstColumn xss = [x | (x:_) <- xss]

shoutOutLoud :: String -> String
shoutOutLoud s = unwords [replicate 3 h ++ xs | (h:xs) <- words s]

pad :: String -> String -> (String, String)
pad s1 s2 = (capitalize $ pad' l s1, capitalize $ pad' l s2)
  where pad' n s         = take n $ s ++ repeat ' '
        capitalize (h:t) = toUpper h : t
        l                = if length s1 > length s2 then length s1 else length s2 


pad2 :: String -> String -> (String, String)
pad2 s1 s2 = 
  let pad' n s         = take n $ s ++ repeat ' '
      capitalize (h:t) = toUpper h : t
      l                = if length s1 > length s2 then length s1 else length s2 
  in (capitalize $ pad' l s1, capitalize $ pad' l s2)


consString p c = "The pair" ++ case p of 
                       (1, 1) -> " contains two ones"
                       (1, _) -> " contains one one"
                       (_, 1) -> " contains one one"
                       _      -> " contains no ones"
                       ++ " and the second element of the list is " ++ case c of
                       (_:x:_) -> show x
                       _       -> "nonexisting"


