module Interpret where

import Data.List
import Parser(Mode(..), parseExpr)
import System.Environment
import System.IO

help :: String
help = "Usage: ./makefile [-option] <source file?> <destination file?>\n"
  ++ "-option: defaults to \"-css\"\n"
  ++ "\t-t: Print tokens\n"
  ++ "\t-a: Prints AST\n"
  ++ "\t-css: Compile stdin to stdout \n"
  ++ "\t-csf: Compile stdin to file\n"
  ++ "\t-cfs: Compile file to stdout\n"
  ++ "\t-cff: Compile file to file\n"
  ++ "\t-h: Help"

wstrip :: String -> String
wstrip str = filter (not . flip elem "\n") str

interpret :: Mode -> IO ()
interpret StdinStdout = do
  putStr "λ> "
  hFlush stdout
  a <- getLine
  let b = wstrip a
  if b == []
    then interpret StdinStdout
    else if ":quit" `isInfixOf` b
      then return ()    
      else do { result <- parseExpr b
              ; print $ result
              ; interpret StdinStdout
              }

interpret (StdinFile f) = do
  putStr ""

run :: Mode -> IO ()
run (Help)          = putStrLn help
run (TokenPrint f)  = putStrLn "tokens"
run (AstPrint f)    = putStrLn "ast"
run (StdinStdout)   = interpret StdinStdout
run (StdinFile d)   = interpret StdinStdout
run (FileStdout s)  = interpret StdinStdout
run (FileFile s d)  = interpret StdinStdout