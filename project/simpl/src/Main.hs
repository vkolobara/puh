module Main where

main :: IO ()
main = do
  putStrLn "hello world"

data Expression
  = Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
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

data Statement
  = Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Part 01 -----------------------------------------

extend :: State -> String -> Int -> State
extend state var val = newState
  where newState x | x == var  = val
                   | otherwise = state x


empty :: State
empty = empty'
  where empty' _ = 0

-- Part 02 -----------------------------------------

evalE :: State -> Expression -> Int
evalE _ (Val x)           = x
evalE s (Var x)           = s x
evalE s (Op e1 Plus e2)   =             (evalE s e1) +     (evalE s e2)
evalE s (Op e1 Minus e2)  =             (evalE s e1) -     (evalE s e2)
evalE s (Op e1 Times e2)  =             (evalE s e1) *     (evalE s e2)
evalE s (Op e1 Divide e2) =             (evalE s e1) `div` (evalE s e2)
evalE s (Op e1 Gt e2)     = boolToInt $ (evalE s e1) >     (evalE s e2)
evalE s (Op e1 Ge e2)     = boolToInt $ (evalE s e1) >=    (evalE s e2)
evalE s (Op e1 Lt e2)     = boolToInt $ (evalE s e1) <     (evalE s e2)
evalE s (Op e1 Le e2)     = boolToInt $ (evalE s e1) <=    (evalE s e2)
evalE s (Op e1 Eql e2)    = boolToInt $ (evalE s e1) ==    (evalE s e2)

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

-- Part 03 -----------------------------------------

data DietStatement
  = DAssign String Expression
  | DIf Expression DietStatement DietStatement
  | DWhile Expression DietStatement
  | DSequence DietStatement DietStatement
  | DSkip
  deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign var e)        = DAssign var e
desugar (Incr var)            = DAssign var (Op (Var var) Plus (Val 1))
desugar (If e th el)          = DIf e (desugar th) (desugar el)
desugar (While e st)          = DWhile e (desugar st)
desugar (For init cond up st) = DSequence (desugar init)
                                (DWhile cond (DSequence (desugar st) (desugar up)))
desugar (Sequence s1 s2)      = DSequence (desugar s1) (desugar s2)
desugar Skip                  = DSkip


-- Part 04 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple s (DAssign var e)   = extend s var (evalE s e)
evalSimple s (DIf e th el)     | checkCondition s e = evalSimple s th
                               | otherwise          = evalSimple s el
evalSimple s w@(DWhile e st)   | checkCondition s e = evalSimple newS w
                               | otherwise          = s
  where newS = evalSimple s st
evalSimple s (DSequence s1 s2) = evalSimple (evalSimple s s1) s2
evalSimple s DSkip             = s

checkCondition :: State -> Expression -> Bool
checkCondition s e = evalE s e == 1

run :: State -> Statement -> State
run s = evalSimple s . desugar

-- Part 05 -----------------------------------------

parse :: String -> Maybe Statement
parse = undefined

-- Programs ----------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input
  for (Out := 1; In > 0; In := In - 1) {
      Out := In * Out
  }
-}

factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{-
  Calculate the floor of the square root of the input
  B := 0;
  while (A >= B * B) {
      B++
  };
  B := B - 1
-}

squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{-
  Calculate the nth Fibonacci number

  F0 := 1;
  F1 := 1;

  if (In == 0) {
      Out := F0
  } else {
      if (In == 1) {
          Out := F1
      } else {
          for (C := 2; C <= In; C++) {
              T  := F0 + F1;
              F0 := F1;
              F1 := T;
              Out := T
          }
      }
  }

-}

fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
