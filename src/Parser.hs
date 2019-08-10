module Parser
  ( Mode(..)
  , parseFile
  ) where

import Ast

import Control.Monad (void)
import Text.Parsec.String (Parser)

data Mode
  = Help
  | TokenPrint String
  | AstPrint String
  | StdinStdout
  | StdinFile String
  | FileStdout String
  | FileFile String String
  deriving (Eq, Show)

program :: Parser Program


parseFile :: String -> IO Program
parseFile file = do
  input <- readFile file
  case parse program "" input of
    Left err -> print err >> fail "Failed to parse"
    Right r  -> return r