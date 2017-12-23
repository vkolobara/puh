module Expression
where

data Expression
  = Var String
  | Val Int
  | Op  Expression Bop Expression
  deriving (Show, Eq)

data Bop
  = Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

type State = String -> Int
------------------------------------------

extend :: State -> String -> Int -> State
extend state var val = state'
  where state' x | x == var  = val
                 | otherwise = state x

empty :: State
empty = (\_ -> 0)

evalE :: State -> Expression -> Int
evalE _ (Val x)           = x
evalE s (Var x)           = s x
evalE s (Op e1 Plus e2)   =             evalE s e1 +     evalE s e2
evalE s (Op e1 Minus e2)  =             evalE s e1 -     evalE s e2
evalE s (Op e1 Times e2)  =             evalE s e1 *     evalE s e2
evalE s (Op e1 Divide e2) =             evalE s e1 `div` evalE s e2
evalE s (Op e1 Gt e2)     = boolToInt $ evalE s e1 >     evalE s e2
evalE s (Op e1 Ge e2)     = boolToInt $ evalE s e1 >=    evalE s e2
evalE s (Op e1 Lt e2)     = boolToInt $ evalE s e1 <     evalE s e2
evalE s (Op e1 Le e2)     = boolToInt $ evalE s e1 <=    evalE s e2
evalE s (Op e1 Eql e2)    = boolToInt $ evalE s e1 ==    evalE s e2

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0
