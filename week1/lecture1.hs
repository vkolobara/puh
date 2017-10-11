import Data.Char
import Data.List

x = 2

inc x = x+1

digits2Number x y = x*10 + y

y = inc 2
z = digits2Number 4 2

w = 25 `div` 2

name = "Konj"
letter = 'H'

s = "One " ++ " two " ++ " three."

n1 = length "Ovo je super string"
n2 = length s

condDec x = if x > 0 then x-1 else x

foo x = (if even x then x*2 else 2) + 1

bigNumber x = x>=1000

merge s1 s2
  | s1 < s2   = s1 ++ " is not " ++ s2
  | otherwise = s1 ++ " is " ++ s2

showSalary amount bonus
  | bonus /= 0 = "Salary is " ++ show amount ++ ", and a bonus " ++ show bonus
  | otherwise = "Salary is " ++ show amount

-- Exercise ----------------------
concat3 s1 s2 s3 
  | length s2 < 2 = s1 ++ s3
  | otherwise = s1 ++ s2 ++ s3

showSalary' amount bonus 
  | amount >= 0 = if bonus /= 0 then show amount ++ ", " ++ show bonus else show amount 
  | otherwise = "Salary is negative!"
----------------------------------
