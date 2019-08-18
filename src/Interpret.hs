module Interpret where

import Data.List
import Parser (Mode(..), parseExpr, parseFile)
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
wstrip str = filter (not . flip elem "\t") str

interpret :: Mode -> IO ()
interpret mode = do
  putStr "λ> "
  hFlush stdout
  a <- getLine
  let input = wstrip a
  if input == []
    then interpret mode
    else if ":quit" `isInfixOf` input
      then return ()    
      else do { result <- parseExpr input
              ; case mode of
                  StdinStdout -> print result
                  StdinFile f -> writeFile f $ (show result) ++ "\n"
                  _           -> fail "Invalid mode"
              ; interpret mode
              }

compile :: Mode -> IO ()
compile mode = do
  result <- case mode of
    FileStdout s -> parseFile s
    FileFile s _ -> parseFile s
    _            -> fail "Invalid mode"
  case mode of
    FileStdout _ -> print result
    FileFile _ d -> writeFile d $ (show result) ++ "\n"
    _            -> fail "Invalid mode"


run :: Mode -> IO ()
run (Help)          = putStrLn  $ help
run (TokenPrint f)  = putStrLn  $ "tokens"
run (AstPrint f)    = putStrLn  $ "ast"
run (StdinStdout)   = interpret $ StdinStdout
run (StdinFile d)   = interpret $ StdinFile d
run (FileStdout s)  = compile   $ FileStdout s
run (FileFile s d)  = compile   $ FileFile s d