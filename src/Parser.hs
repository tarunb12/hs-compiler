module Parser
  ( Mode(..)
  , parseExpr
  , parseFile
  ) where

import Ast

import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)
import Text.Parsec (parse)
import Text.Parsec.Char (char, digit, oneOf, satisfy)
import Text.Parsec.Combinator (eof, many1)
import Text.Parsec.String (Parser)

data Mode
  = Help
  | TokenPrint String
  | AstPrint String
  | StdinStdout
  | StdinFile String
  | FileStdout String
  | FileFile String String
  | Run
  deriving (Eq, Show)

whiteSpace :: Parser ()
whiteSpace = void $ many $ oneOf " \n\t"

integer :: Parser Expr
integer = lexeme $ do
  int <- many1 digit
  return $ Literal $ LInt $ read int

maybeAdd :: Expr -> Parser Expr
maybeAdd expr0 = 
  let add expr0 = do { void $ lexeme $ char '+'
                     ; expr1 <- expr
                     ; maybeAdd $ BinOp Add expr0 expr1
                     }
  in  add expr0
  <|> return expr0

variable :: Parser Expr
variable = 
  let firstChar = satisfy $ \a -> isLetter a || a == '_'
      restChar  = satisfy $ \a -> isLetter a || a == '_' || isDigit a
  in lexeme $ do
    fst <- firstChar
    rest <- many restChar
    return $ Id (fst : rest)

parens :: Parser Expr
parens = lexeme $ do
  void $ lexeme $ char '('
  exp <- expr
  void $ lexeme $ char ')'
  return $ Parens exp

-- literal :: Parser Literal
-- literal = do
--   int <- integer

literal :: Parser Expr
literal =  integer

term :: Parser Expr
term =  literal
    <|> variable
    <|> parens

expr :: Parser Expr
expr = do
  e <- term
  maybeAdd e

-- Lexer for interpreter
lexeme :: Parser a -> Parser a
lexeme parser = do
  x <- parser
  whiteSpace
  return x
  
-- parseExpr :: String -> Mode -> IO Expr
parseExpr :: String -> IO Expr
parseExpr e = case parse expr "" e of
  Left err -> print err >> fail "Failed to parse"
  Right r  -> return r  

parseFile :: String -> IO Expr -- IO Program
parseFile file = do
  input <- readFile file
  case parse expr "" input of
    Left err -> print err >> fail "Failed to parse"
    Right r  -> return r