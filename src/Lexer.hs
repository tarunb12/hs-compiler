module Lexer where

import Ast

import Control.Monad (void)
import Text.Parsec.Char (digit)
import Text.Parsec.Combinator (many1)
import Text.Parsec.String (Parser)

whiteSpace :: Parser ()
whiteSpace = void $ many $ oneOf " \n\t"

integer :: Parser Literal
integer = lexeme $ do
  n <- many1 digit
  return $ LInt $ read n

lexeme :: Parser a -> Parser a
lexeme parser = do
  x <- p
  whiteSpace
  return x