module Parser where

import Expression
import Statement

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

import Data.Either

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) (Just)

parse :: String -> Maybe Statement
parse = eitherToMaybe . Text.ParserCombinators.Parsec.parse parseStatement ""

spaces :: Parser()
spaces = skipMany space

number :: Parser Int
number = try negativeNum <|> read <$> many1 digit

negativeNum :: Parser Int
negativeNum = do
  char '-'
  num <- read <$> many1 digit
  return (-num)

word :: Parser String
word = many1 $ choice [letter, digit]

operator :: Parser String
operator = choice [try $ string ">=", try $ string "<=", try $ string "==", string "*", string "/"
                  ,string ">", string "+", string "<", string "-"]

parseAssignment :: Parser Statement
parseAssignment = do
  (Var var) <- parseVariable
  optional spaces
  string ":="
  optional spaces
  expr <- parseExpression
  return (Assign var expr)

parseIncr :: Parser Statement
parseIncr = do
  (Var var) <- parseVariable
  string "++"
  return (Incr var)

parseIf :: Parser Statement
parseIf = do
  string "if"
  optional spaces
  char '('
  optional spaces
  expr <- parseExpression
  optional spaces
  char ')'
  optional spaces
  char '{'
  optional spaces
  ifSt <- parseStatement
  optional spaces
  char '}'  
  optional spaces
  string "else"
  optional spaces
  char '{'
  optional spaces
  elSt <- parseStatement
  optional spaces
  char '}'
  return (If expr ifSt elSt)

parseWhile :: Parser Statement
parseWhile = do
  string "while"
  optional spaces
  char '('
  optional spaces
  expr <- parseExpression
  optional spaces
  char ')'
  optional spaces
  char '{'
  optional spaces
  st <- parseStatement
  optional spaces
  char '}'
  return (While expr st)
  
parseFor :: Parser Statement
parseFor = do
  string "for"
  optional spaces
  char '('
  optional spaces
  init <- parseStatementNoSeq
  optional spaces
  char ';'
  optional spaces
  cond <- parseExpression
  optional spaces
  char ';'
  optional spaces
  upd <- parseStatementNoSeq
  optional spaces
  char ')'
  optional spaces
  char '{'
  optional spaces
  body <- parseStatement
  optional spaces
  char '}'
  return (For init cond upd body)

parseSkip :: Parser Statement
parseSkip = do
  string "skip"
  return Skip

parseSequence :: Parser Statement
parseSequence = do
  st1 <- parseStatementNoSeq
  optional spaces
  char ';'
  optional spaces
  st2 <- parseStatement
  return (Sequence st1 st2)  

parseStatementNoSeq :: Parser Statement
parseStatementNoSeq = try parseAssignment <|> try parseIncr <|> try parseIf <|> try parseWhile
                  <|> try parseFor <|> try parseSkip

parseStatement :: Parser Statement
parseStatement = try parseSequence <|> parseStatementNoSeq

parseVariable :: Parser Expression
parseVariable = do
  var <- word
  return (Var var)
  
parseValue :: Parser Expression
parseValue = do
  val <- number
  return (Val val)

parseOp :: Parser Expression
parseOp = do
  e1 <- term
  optional spaces
  op <- operator
  optional spaces
  e2 <- parseExpression
  return (Op e1 (stringToBop op) e2)

parseExpression :: Parser Expression
parseExpression = try parseOp <|> term

term :: Parser Expression
term = try parseValue <|> parseVariable

stringToBop :: String -> Bop
stringToBop "+"  = Plus
stringToBop "-"  = Minus
stringToBop "*"  = Times
stringToBop "/"  = Divide
stringToBop ">"  = Gt
stringToBop ">=" = Ge
stringToBop "<"  = Lt
stringToBop "<=" = Le
stringToBop "==" = Eql
stringToBop _    = error "unknown op"
