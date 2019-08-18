module Parser
  ( Mode(..)
  , parseExpr
  , parseFile
  ) where

import Ast
import Lexer (Token(..), Parser, boolean, character, id', integer, lexer, lpar, rpar, string', ws)

import Control.Applicative ((<|>), many)
import Control.Monad ((>=>), void)
import Text.Parsec (ParseError, runParser)
import Text.Parsec.Char (char, digit, newline, oneOf, satisfy)
import Text.Parsec.Combinator (between, eof, option, many1)

data Mode
  = Help
  | TokenPrint String
  | AstPrint String
  | StdinStdout
  | StdinFile String
  | FileStdout String
  | FileFile String String
  deriving (Eq, Show)

-- do blocks for pattern matching tokens

-- parseExpr :: String -> Mode -> IO Expr
parseExpr :: String -> IO Program
parseExpr e = case parseProg e of
  Left err -> print err >> fail "Failed to parse"
  Right r  -> return r  

parseFile :: String -> IO Program -- IO Program
parseFile file = do
  input <- readFile file
  case parseProg input of
    Left err -> print err >> fail "Failed to parse"
    Right r  -> return r

parseProg :: String -> Either ParseError Program
parseProg =  runParser (lexer <* eof) () ""
         >=> runParser (program <* eof) () ""

program :: Parser Program
program = statement >>= \p -> return $ Program [p]

statement :: Parser Statement
statement =  expr

-- Parser Statement
expr :: Parser Statement
expr = do 
  e0 <- term 
  option (Expr e0) (parens expr >>= \e1 -> return e1)

term :: Parser Expr
term =  var
    <|> (literal >>= \lit -> return $ Literal lit)
    <|> parens term

var :: Parser Expr
var = Id <$> id'

literal :: Parser Literal
literal =  bool
       <|> char'
       <|> int
       <|> str

bool, char', int, str :: Parser Literal
bool  = boolean   >>= \b -> return $ LBool b
char' = character >>= \c -> return $ LChar c
int   = integer   >>= \i -> return $ LInt i
str   = string'   >>= \s -> return $ LString s

parens :: Parser a -> Parser a 
parens = between lpar rpar