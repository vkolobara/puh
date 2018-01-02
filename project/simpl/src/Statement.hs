module Statement
where

import Expression

data Statement
  = Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement  
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

data DietStatement
  = DAssign   String     Expression
  | DIf       Expression    DietStatement  DietStatement
  | DWhile    Expression    DietStatement
  | DSequence DietStatement DietStatement
  | DSkip
  deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign var e)        = DAssign var e
desugar (Incr var)            = DAssign var (Op (Var var) Plus (Val 1))
desugar (If e th el)          = DIf e (desugar th) (desugar el)     
desugar (While e st)          = DWhile e (desugar st)
desugar (For init cond up st) 
  = DSequence (desugar init)
    (DWhile cond (DSequence (desugar st) (desugar up)))
desugar Skip = DSkip
desugar (Sequence s1 s2) = DSequence (desugar s1) (desugar s2)


evalSimple :: State -> DietStatement -> State
evalSimple s (DAssign var e)   = extend s var (evalE s e)
evalSimple s (DIf e th el)     = if checkCondition s e 
                                 then evalSimple s th 
                                 else evalSimple s el
evalSimple s w@(DWhile e st)   = if checkCondition s e
                                 then evalSimple (evalSimple s st) w
                                 else s
evalSimple s (DSequence s1 s2) = evalSimple (evalSimple s s1) s2
evalSimple s DSkip             = s

checkCondition :: State -> Expression -> Bool
checkCondition s e = evalE s e == 1

run :: State -> Statement -> State
run s = evalSimple s . desugar
