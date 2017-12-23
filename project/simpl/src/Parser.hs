module Parser where

import Expression
import Statement

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

parse :: String -> Maybe Statement
parse = undefined

languageDef :: Token.LanguageDef ()
languageDef = Token.LanguageDef
  { Token.commentStart    = ""
  , Token.commentEnd      = ""
  , Token.commentLine     = ""
  , Token.nestedComments  = False
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> oneOf "_"
  , Token.opStart         = oneOf ":!#$%^*+./<=>?@\\^"
  , Token.opLetter        = oneOf ":!#$%^*+./<=>?@\\^"
  , Token.reservedNames   = [ "if" 
                            , "else"
                            , "for"
                            , "while"
                            ]
  , Token.reservedOpNames = [ "+", "-", "*", "/", ":="
                            , "<", ">", "<=", ">=", "=="
                            ]
  , Token.caseSensitive   = True
  }

lexer :: Token.TokenParser()
lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
parens = Token.parens lexer
reserved = Token.reserved lexer
semiSep = Token.semiSep lexer
reservedOp = Token.reservedOp lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer

operators = [ [Infix (reservedOp "+"  >> return (flip(Op) Plus)) AssocLeft] 
            , [Infix (reservedOp "*"  >> return (flip(Op) Times)) AssocLeft]  
            , [Infix (reservedOp "-"  >> return (flip(Op) Minus)) AssocLeft]  
            , [Infix (reservedOp "/"  >> return (flip(Op) Divide)) AssocLeft]  
            , [Infix (reservedOp ">"  >> return (flip(Op) Gt)) AssocLeft]  
            , [Infix (reservedOp ">=" >> return (flip(Op) Ge)) AssocLeft]  
            , [Infix (reservedOp "<"  >> return (flip(Op) Lt)) AssocLeft]  
            , [Infix (reservedOp "<=" >> return (flip(Op) Le)) AssocLeft]  
            , [Infix (reservedOp "==" >> return (flip(Op) Eql)) AssocLeft]  
            ]

expression = buildExpressionParser operators term

term = parens expression
     <|> liftM Var identifier
