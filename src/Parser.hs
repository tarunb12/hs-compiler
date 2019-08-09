module Parser
  ( Mode(..)
  , parseFile
  ) where

import Ast

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Mode
  = Help
  | TokenPrint String
  | AstPrint String
  | StdinStdout
  | StdinFile String
  | FileStdout String
  | FileFile String String
  deriving (Eq, Show)

languageDef =
  emptyDef { Token.commentStart   = "/*"
           , Token.commentEnd     = "*/"
           , Token.commentLine    = "//"
           , Token.identStart     = letter
           , Token.identLetter    = alphaNum
           , Token.reservedNames  =
              [ "if"
              , "then"
              , "else"
              , "while"
              , "do"
              , "skip"
              , "true"
              , "false"
              , "not"
              , "and"
              , "or"
              ]
           , Token.reservedOpNames  =
              [ "+", "-", "*", "/", "="
              , "<", ">", "and", "or", "not"
              ]
           }
  
lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
integer    = Token.integer    lexer
parens     = Token.parens     lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
semi       = Token.semi       lexer
whiteSpace = Token.whiteSpace lexer

parser :: Parser Statement
parser = whiteSpace >> statement

statement :: Parser Statement
statement =  parens statement
         <|> statementList

statementList = do
  list <- sepBy1 statement' semi
  return $ if length list == 1 then head list else Block list

statement' :: Parser Statement
statement' =  assignStmt

assignStmt :: Parser Statement
assignStmt = do
  var <- identifier
  reservedOp "="
  expr <- aExpression
  return $ Assign var expr

aExpression :: Parser Expr
aExpression = buildExpressionParser aOperators aTerm

aOperators = [ [Infix  (reservedOp "*"   >> return (BinOp Mul)) AssocLeft]
             , [Infix  (reservedOp "/"   >> return (BinOp Div)) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (BinOp Sub)) AssocLeft]
             ]

aTerm =  parens aExpression
     <|> liftM Id identifier

parseFile :: String -> IO Statement
parseFile file = do
  program <- readFile file
  case parse parser "" program of
    Left e  -> print e >> fail "parser error"
    Right r -> return r